import akka.actor.{Actor, ActorSystem, Props}

object ActorsIntro extends App {
  val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

  class WordCountActor extends Actor {
    var totalWords = 0

    override def receive: Receive = {
      case message: String =>
        println(s"[word counter] I have received $message")
        totalWords += message.split(' ').length
      case msg => println(s"[word counter] I cannot understand ${msg.toString}")
    }
  }

  val wordCounter = actorSystem.actorOf(Props[WordCountActor], "wordCounter")
  val anotherWordCounter = actorSystem.actorOf(Props[WordCountActor], "anotherWordCounter")

  // Asynchronous so order of printing is random
  wordCounter ! "I am learning akka and it's pretty damn cool"
  anotherWordCounter ! "I'm a different message"

  // can't do this/instantiate directly
  // new WordCountActor

  object Person {
    def props(name: String): Props = Props(new Person(name))
  }

  class Person(name: String) extends Actor {
    override def receive: Receive = {
      case "hi" => println(s"Hi, my name is $name.")
      case _ => ()
    }
  }

  // val person = actorSystem.actorOf(Props(new Person("Bob")))
  val person = actorSystem.actorOf(Person.props("Bob"))
  person ! "hi"
}
