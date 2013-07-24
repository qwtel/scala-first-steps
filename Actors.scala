import java.net.UnknownHostException
import scala.actors._

object SillyActor extends Actor {
  def act() {
    for (i <- 1 to 5) {
      println("I'm acting!")
      Thread.sleep(1000)
    }
  }
}

object SeriousActor extends Actor {
  def act() {
    for (i <- 1 to 5) {
      println("To be or not to be.")
      Thread.sleep(1000)
    }
  }
}

object EchoActor extends Actor {
  def act() {
    while (true) {
      receive {
        case msg => println("received message: " + msg)
      }
    }
  }
}

object NameResolver extends Actor {
  import java.net.{InetAddress, UnknownHostException}

  def act() {
    loop {
      react {
        case (name: String, actor: Actor) =>
          actor ! getIp(name)
        case msg =>
          println("Unhandled message: "+ msg)
      }
    }
  }

  private def getIp(name: String): Option[InetAddress] = {
    try {
      Some(InetAddress.getByName(name))
    } catch {
      case _: UnknownHostException => None
    }
  }
}
