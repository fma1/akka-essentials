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
  val mom = actorSystem.actorOf(Props[Mom], "mom")

  mom ! MomStart(fussyKid)
}
