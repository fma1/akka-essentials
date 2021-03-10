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

  // Exercises
  final val INCREMENT = "INCREMENT"
  final val DECREMENT = "DECREMENT"
  final val PRINT = "PRINT"

  class CounterActor extends Actor {
    private val count = 0

    override def receive: Receive = onMessage(count)

    private def onMessage(count: Int): Receive = {
      case INCREMENT =>
        context.become(onMessage(count + 1))
      case DECREMENT =>
        context.become(onMessage(count - 1))
      case PRINT =>
        println(s"Current count: $count")
    }
  }

  val counterActor = system.actorOf(Props[CounterActor], "counterActor")

  counterActor ! INCREMENT
  counterActor ! INCREMENT
  counterActor ! INCREMENT
  counterActor ! DECREMENT
  counterActor ! PRINT

  final val WITHDRAW = "WITHDRAW"
  final val DEPOSIT = "DEPOSIT"
  final val STATEMENT = "STATEMENT"

  // Like Withdrawal | Deposit | Statement in Javascript
  sealed trait BankActions
  case class Withdrawal(amount: Int) extends BankActions
  case class Deposit(amount: Int) extends BankActions
  case object Statement extends BankActions

  // TODO: Using Ints instead of Float/Double due to precision, should change in future to work like Floats
  class BankActor extends Actor {
    private val balance = 0

    override def receive: Receive = onMessage(balance)

    private def onMessage(balance: Int): Receive = {
      case Withdrawal(amount) =>
        if (amount > balance) {
          context.sender ! s"Failed to withdraw $amount"
        } else {
          context.become(onMessage(balance + amount))
          context.sender ! s"Succeeded withdrawing amount"
        }
      case Deposit(amount) =>
        // TODO: Currently ignoring Int.MaxValue and overflow
        context.become(onMessage(balance + amount))
        context.sender ! s"Succeeded depositing $amount"
      case Statement =>
        println(s"Balance: $balance")
    }
  }

  case class SendBankActions(lst: List[BankActions], actorRef: ActorRef)

  class ForwardActor extends Actor {
    override def receive: Receive = {
      case SendBankActions(lst, actorRef) =>
        lst.foreach(bankAction => actorRef ! bankAction)
      case message: String => println(s"${context.self} I have received message \'$message\'")
      case anything: AnyRef => println(anything)
    }
  }

  val bankActor = system.actorOf(Props[CounterActor], "bankActor")
  val forwardActor = system.actorOf(Props[CounterActor], "forwardActor")

  // TODO: FIXME so it prints out
  val bankActions = List(Deposit(20), Withdrawal(20), Withdrawal(120), Statement)
  forwardActor ! SendBankActions(bankActions, bankActor)
}