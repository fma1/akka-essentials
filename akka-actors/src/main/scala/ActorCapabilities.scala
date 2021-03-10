import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorCapabilities extends App {
  class SimpleActor extends Actor {
    override def receive: Receive = {
      case "Hi!" => context.sender ! "Hello, there!" // replying to sender
      case message: String => println(s"${context.self} I have received $message")
      case number: Int => println(s"[simple actor] I have received a NUMBER: $number")
      case SpecialMessage(contents) => println(s"[simple actor] I have received something SPECIAL: $contents")
      case SendMessageToYourself(contents) => self ! contents
      case SayHiTo(ref) => ref ! "Hi!" // alice implicitly passed as sender
      case WirelessPhoneMessage(contents, ref) => ref forward contents + "s" // keep original sender of WirelessPhoneMessage
    }
  }

  val system: ActorSystem = ActorSystem("ActorCapabilities")
  val simpleActor: ActorRef = system.actorOf(Props[SimpleActor], "simpleActor")

  simpleActor ! "hello, world"

  // 1 - Messages can be of any type
  // a) Messages must be IMMUTABLE
  // b) Messages must be SERIALIZABLE
  // in practice use case classes and case objects

  simpleActor ! 42

  case class SpecialMessage(contents: String)

  simpleActor ! SpecialMessage("some special message")

  // 2 - Actors have context about their environment and themselves
  // context.self === `this` in OOP

  case class SendMessageToYourself(contents: String)

  simpleActor ! SendMessageToYourself("message to myself")

  // 3 - Actors can reply to messages
  val alice: ActorRef = system.actorOf(Props[SimpleActor], "alice")
  val bob: ActorRef = system.actorOf(Props[SimpleActor], "bob")

  case class SayHiTo(ref: ActorRef)

  alice ! SayHiTo(bob)

  // 4 - dead letters
  alice ! "Hi!"

  // 5 - forwarding
  // D->A->B
  // forwarding = sending a message with original sender
  case class WirelessPhoneMessage(contents: String, ref: ActorRef)
  alice ! WirelessPhoneMessage("Hi", bob)
}
