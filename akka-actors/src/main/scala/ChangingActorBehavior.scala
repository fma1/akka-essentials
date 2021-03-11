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
    var state = HAPPY
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
      case Food(VEGETABLE) => context.become(sadReceive)
      case Food(CHOCOLATE) => ()
      case Ask(_) => sender() ! KidAccept
    }

    def sadReceive: Receive = {
      case Food(VEGETABLE) => ()
      case Food(CHOCOLATE) => context.become(happyReceive)
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
        kidRef ! Food(VEGETABLE)
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
}
