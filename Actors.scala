import java.net.{InetAddress, UnknownHostException}
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

case class LookupIP(hostname: String, requester: Actor)
case class LookupResult(name: String, address: Option[InetAddress])

object NameResolver extends Actor {
  def act() {
    loop {
      react {
        case LookupIP(name, actor) =>
          actor ! LookupResult(name, getIp(name))
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
