object RoboShop extends java.lang.Iterable[Android] {

  import scala.collection._
  private var set = mutable.Set[Android]()

  def insert(robo: Android) =
    if(robo.skin != null && robo.software != null && robo.kit != null)
      if (!set.contains(robo)) set += robo
      else {
        val old = set.find(_.id == robo.id).get
        (old, robo) match {
          case (old: Servant, robo: Servant) => replace(old, robo)
          case (old: Worker, robo: Worker) => replace(old, robo)
          case (old: Guard, robo: Guard) => replace(old, robo)
          case _ => throw new Exception("Can't replace " + old.getClass + " with " + robo.getClass)
        }
      }
    else
      throw new Exception("Android Act")

  private def replace(old: Android, a: Android) = {
    set -= old
    set += a
  }

  def find(id: Int): String =
    set.find(_.id == id) match {
      case Some(android) => android.toString
      case None => null
    }

  def iterator() =
    new java.util.Iterator[Android] {
      def hasNext: Boolean = ???
      def next(): Android = ???
      def remove() {}
    }

  def main(args: Array[String]) = {
    val id= 1
    insert(
      new Entertainer(
        id,
        SkinTouchSensitive(id),
        SoftwareEntertainer(id, Certificate(1)),
        ActorSensorKit(id, 0.75)
      )
    )

    insert(
      new Combatant(
        id,
        SkinArmored(id),
        SoftwareCombatant(id, Certificate(5)),
        ActorSensorKit(id, 75)
      )
    )

    println(find(1))
  }
}

object Android {
  private var id = 0
  def generateId() = {
    id += 1
    id
  }
}

abstract class Android(val id: Int, k: ActorSensorKit) {

  final def valid(id: Int): Boolean = this.id == id

  val skin: Skin
  val software: Software
  lazy val kit = (this.software.certificate, k) match {
    case (Certificate(3), ActorSensorKit(id, capacity)) if (valid(id) && capacity <= 5.0) => k
    case (Certificate(4), ActorSensorKit(id, capacity)) if (valid(id) && capacity <= 10.0) => k
    case _ => null
  }

  override def hashCode = id

  override def equals(that: Any) = {
    that match {
      case that: Android => this.id == that.id
      case _ => false
    }
  }

  override def toString =
    this.getClass.toString.split(" ").last + " (Id: "+id+"):\n" +
    "Skin: " + skin.toString + "\n" +
    "Software: " + software.toString + "\n" +
    "ActorSensorKit: " + kit.toString
}

abstract class Servant(i: Int, s: Skin, k: ActorSensorKit) extends Android(i, null) {
  lazy val skin = s match {
    case SkinTouchSensitive(id) if valid(id) => s
    case _ => null
  }

  override lazy val kit = k match {
    case ActorSensorKit(id, capacity) if (valid(id) && capacity <= 1.0) => k
    case _ => null
  }
}

abstract class Worker(i: Int, s: Skin, k: ActorSensorKit) extends Android(i, k) {
  lazy val skin = s match {
    case SkinTouchSensitive(id) if valid(id) => s
    case SkinSolid(id) if valid(id) => s
    case _ => null
  }
}

abstract class Guard(i: Int, s: Skin, k: ActorSensorKit) extends Android(i, k) {
  lazy val skin = s match {
    case _: Skin if valid(s.id) => s
    case _ => null
  }
}

class Assistant(i: Int, s: Skin, sw: Software, k: ActorSensorKit) extends Servant(i, s, k) {
   lazy val software = sw match {
    case SoftwareAssistant(id, Certificate(level)) if (valid(id) && level <= 2) => sw
    case _ => null
  }
}

class Entertainer(i: Int, s: Skin, sw: Software, k: ActorSensorKit) extends Servant(i, s, k) {
  lazy val software = sw match {
    case SoftwareEntertainer(id, Certificate(1)) if valid(id) => sw
    case _ => null
  }
}

class ConstructionWorker(i: Int, s: Skin, sw: Software, k: ActorSensorKit) extends Worker(i, s, k) {
  lazy val software = sw match {
    case SoftwareConstructionWorker(id, Certificate(level)) if (valid(id) && level >= 3 && level <= 4) => sw
    case _ => null
  }
}

class ServiceEngineer(i: Int, s: Skin, sw: Software, k: ActorSensorKit) extends Worker(i, s, k) {
  lazy val software = sw match {
    case SoftwareServiceEngineer(id, Certificate(level)) if (valid(id) && level >= 3 && level <= 4) => sw
    case _ => null
  }
}

class TransportWorker(i: Int, s: Skin, sw: Software, k: ActorSensorKit) extends Worker(i, s, k) {
  lazy val software = sw match {
    case SoftwareTransportWorker(id, Certificate(level)) if (valid(id) && level >= 3 && level <= 4) => sw
    case _ => null
  }
}

class PropertyGuard(i: Int, s: Skin, sw: Software, k: ActorSensorKit) extends Guard(i, s, k) {
  lazy val software = sw match {
    case SoftwarePropertyGuard(id, Certificate(4)) if valid(id) => sw
    case _ => null
  }
}

class BodyGuard(i: Int, s: Skin, sw: Software, k: ActorSensorKit) extends Guard(i, s, k) {
  lazy val software = sw match {
    case SoftwareBodyGuard(id, Certificate(4)) if valid(id) => sw
    case _ => null
  }
}

class Combatant(i: Int, s: Skin, sw: Software, k: ActorSensorKit) extends Guard(i, s, null) {
  lazy val software = sw match {
    case SoftwareCombatant(id, Certificate(5)) if valid(id) => sw
    case _ => null
  }

  override lazy val kit = k match {
    case ActorSensorKit(id, _) if valid(id) => k
    case _ => null
  }
}

sealed abstract class Skin {
  val id: Int
}
case class SkinTouchSensitive(id: Int) extends Skin
case class SkinSolid(id: Int) extends Skin
case class SkinArmored(id: Int) extends Skin

sealed abstract class Software {
  val id: Int
  val certificate: Certificate
}
case class SoftwareAssistant(id: Int, certificate: Certificate) extends Software
case class SoftwareEntertainer(id: Int, certificate: Certificate) extends Software
case class SoftwareConstructionWorker(id: Int, certificate: Certificate) extends Software
case class SoftwareServiceEngineer(id: Int, certificate: Certificate) extends Software
case class SoftwareTransportWorker(id: Int, certificate: Certificate) extends Software
case class SoftwarePropertyGuard(id: Int, certificate: Certificate) extends Software
case class SoftwareBodyGuard(id: Int, certificate: Certificate) extends Software
case class SoftwareCombatant(id: Int, certificate: Certificate) extends Software

case class Certificate(level: Int) {
  require(level >= 1 && level <= 5)
}

case class ActorSensorKit(id: Int, capacity: Double)
