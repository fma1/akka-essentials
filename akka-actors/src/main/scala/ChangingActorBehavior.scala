import ChangingActorBehavior.FussyKid.{HAPPY, KidAccept, KidReject, SAD}
import ChangingActorBehavior.Mom.{Ask, CHOCOLATE, Food, MomStart, VEGETABLE}
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChangingActorBehavior extends App {
  object FussyKid {
    case object KidAccept
    case object KidReject
    val HAPPY = "happy"
    val SAD = "sad"
  }
  class FussyKid extends Actor {
    var state: String = HAPPY
    override def receive: Receive = {
      case Food(VEGETABLE) =>
        state = SAD
      case Food(CHOCOLATE) =>
        state = HAPPY
      case Ask(_) =>
        if (state == HAPPY) sender() ! KidAccept
        else sender() ! KidReject
    }
  }

  class StatelessFussyKid extends Actor {
    override def receive: Receive = happyReceive

    def happyReceive: Receive =  {
      case Food(VEGETABLE) => context.become(sadReceive, discardOld = false)
      case Food(CHOCOLATE) => ()
      case Ask(_) => sender() ! KidAccept
    }

    def sadReceive: Receive = {
      case Food(VEGETABLE) => context.become(sadReceive, discardOld = false)
      case Food(CHOCOLATE) => context.unbecome()
      case Ask(_) => sender() ! KidReject
    }
  }

  object Mom {
    case class MomStart(kidRef: ActorRef)
    case class Food(food: String)
    case class Ask(message: String)
    val VEGETABLE = "veggies"
    val CHOCOLATE = "chocolate"
  }
  class Mom extends Actor {
    override def receive: Receive = {
      case MomStart(kidRef) =>
        // first 2 veggies push 2 sadReceives onto stack
        // 2 chocolates pop them and then akka uses happyReceive again
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(CHOCOLATE)
        // kidRef ! Food(CHOCOLATE)
        kidRef ! Ask("do you want to play?")
      case KidAccept =>
        println("Yay, my kid is happy!")
      case KidReject =>
        println("My kid is sad, but he's healthy!")
    }
  }

  val actorSystem = ActorSystem("changingActorBehavior")
  val fussyKid = actorSystem.actorOf(Props[FussyKid], "fussyKid")
  val statelessFussyKid = actorSystem.actorOf(Props[StatelessFussyKid], "statelessFussyKid")
  val mom = actorSystem.actorOf(Props[Mom], "mom")

  // mom ! MomStart(fussyKid)

  mom ! MomStart(statelessFussyKid)

  /*
   * Mom receives MomStart
       - kid receives Food(veg) -> kid will change the receiver to sadReceive
       - kid receives Ask(play?) -> kid replies with the sadReceive handler
   * Mom receives KidReject
   */

  /*
   * context.become() has a second parameter
   * true means discard old handler
   * false means push on top of old handler on the stack of handlers
   *
   * Food(veg) -> stack.push(sadReceive)
   * Food(chocolate) -> stack.push(happyReceive)
   *
   * 1. happyReceive
   * 2. sadReceive
   * 3. happyReceive
   *
   * Akka call receiver on top of stack
   * if empty, calls receive()
   *
   * To pop and use old handler, use context.unbecome()
   */

  /* Exercise 2 - a simplified voting system */
  case class Vote(candidate: String)
  case object VoteStatusRequest
  case class VoteStatusReply(candidate: Option[String])
  class Citizen extends Actor {
    val candidate: Option[String] = None

    override def receive: Receive = onMessage(candidate)

    def onMessage(candidate: Option[String]): Receive = {
      case Vote(newCandidate) =>
        context.become(onMessage(Some(newCandidate)), discardOld = false)
      case VoteStatusRequest =>
        sender() ! VoteStatusReply(candidate)
    }
  }
  case class AggregateVotes(citizens: Set[ActorRef])
  class VoteAggregator extends Actor {
    val requestedVotes = 0
    val countedVotes = 0
    val votes: Map[String, Int] = Map()

    val printVotes: Map[String, Int] => Unit =
      (votes: Map[String, Int]) =>
        for ((candidate, numVotes) <- votes) println(s"$candidate - $numVotes")

    val resetAkkaHandler: () => Unit =
      () => context.become(onMessage(0, 0, Map()), discardOld = true)

    override def receive: Receive = onMessage(requestedVotes, countedVotes, votes)

    def onMessage(requestedVotes: Int, countedVotes: Int, votes: Map[String, Int]): Receive = {
      case AggregateVotes(citizens) =>
        context.become(onMessage(citizens.size, countedVotes, votes), discardOld = true)
        citizens.foreach(citizen => citizen ! VoteStatusRequest)
      case VoteStatusReply(candidate) =>
        val countedAllVotes = requestedVotes - 1 == countedVotes
        candidate match {
          case Some(newCandidate) =>
            val newVoteValue = votes.getOrElse(newCandidate, default = 0) + 1
            val newVotes = votes + (newCandidate -> newVoteValue)

            if (countedAllVotes) {
              printVotes(newVotes)
              resetAkkaHandler()
            } else {
              context.become(
                onMessage(requestedVotes, countedVotes + 1, newVotes),
                discardOld = true
              )
            }
          case None =>
            if (countedAllVotes) {
              printVotes(votes)
              resetAkkaHandler()
            } else {
              context.become(
                onMessage(requestedVotes, countedVotes + 1, votes),
                discardOld = true
              )
            }
        }

        if (requestedVotes - 1 == countedVotes) {
        }
    }
  }

  val alice = actorSystem.actorOf(Props[Citizen])
  val bob = actorSystem.actorOf(Props[Citizen])
  val charlie = actorSystem.actorOf(Props[Citizen])
  val daniel = actorSystem.actorOf(Props[Citizen])

  alice ! Vote("Martin")
  bob ! Vote("Jones")
  charlie ! Vote("Roland")
  daniel ! Vote("Roland")

  val voteAggregator = actorSystem.actorOf(Props[VoteAggregator])
  voteAggregator ! AggregateVotes(Set[ActorRef](alice, bob, charlie, daniel))
}
