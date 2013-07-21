object RoboShop extends java.lang.Iterable[Android] {

  import scala.collection._
  private var set = mutable.Set[Android]()

  def insert(robo: Android, s: Skin, sw: Software, a: ActorSensorKit) = {
    robo.skin = s
    robo.software = sw
    robo.kit = a

    if (set.contains(robo)) {
      val old = set.find(_.id == robo.id).get
      (old, robo) match {
        case (old: Servant, robo: Servant) => replace(old, robo)
        case (old: Worker, robo: Worker) => replace(old, robo)
        case (old: Guard, robo: Guard) => replace(old, robo)
        case _ => throw new AndroidActViolation("Can't replace " + old + "\nwith " + robo)
      }
    } else {
      set += robo
    }
  }

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
    var robo = new Entertainer
    robo.id = 1
    insert(
      robo,
      SkinTouchSensitive(robo.id),
      SoftwareEntertainer(robo.id, Certificate(1)),
      ActorSensorKit(robo.id, 0.75)
    )

    var robo2 = new Combatant
    robo2.id = 1
    insert(
      robo2,
      SkinArmored(robo2.id),
      SoftwareCombatant(robo2.id, Certificate(5)),
      ActorSensorKit(robo2.id, 0.75)
    )

    println(find(1))
  }
}

class AndroidActViolation(message: String = null) extends RuntimeException(message)

object Android {
  private var id = 1
  private def generateId() = {
    id += 1
    id - 1
  }
}

abstract class Android {
  var id = Android.generateId()

  private var _s: Skin = _
  private var _sw: Software = _
  private var _a: ActorSensorKit = _

  final def validateId(id: Int): Boolean = this.id == id

  def skin = _s
  def skin_=(s: Skin) =
    if (validateId(s.id) && validateSkin(s)) _s = s
    else throw new AndroidActViolation

  protected def validateSkin(s: Skin): Boolean = true

  def software = _sw
  def software_=(sw: Software) =
    if (validateId(sw.id) && validateSoftware(sw)) _sw = sw
    else throw new AndroidActViolation

  protected def validateSoftware(sw: Software): Boolean = true

  def kit = _a
  def kit_=(a: ActorSensorKit) =
    if (validateId(a.id) && validateKit(a)) _a = a
    else throw new AndroidActViolation

  protected def validateKit(a: ActorSensorKit): Boolean =
    if (software.certificate.level == 3)
      a.capacity <= 5.0
    else if (software.certificate.level == 4)
      a.capacity <= 10.0
    else
      false

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

abstract class Servant extends Android {
  override def skin_=(s: Skin) = s match {
    case s: SkinTouchSensitive => super.skin_=(s)
    case _ => throw new AndroidActViolation
  }

  protected override def validateSoftware(sw: Software): Boolean =
    sw.certificate.level <= 2

  protected override def validateKit(a: ActorSensorKit): Boolean =
    a.capacity <= 1.0
}

class Assistant extends Servant {
  override def software_=(sw: Software) = sw match {
    case sw: SoftwareAssistant => super.software_=(sw)
    case _ => throw new AndroidActViolation
  }
}

class Entertainer extends Servant {
  protected override def validateSoftware(sw : Software): Boolean =
    sw.certificate.level == 1

  override def software_=(sw: Software) = sw match {
    case sw: SoftwareEntertainer => super.software_=(sw)
    case _ => throw new AndroidActViolation
  }
}

abstract class Worker extends Android {
  protected override def validateSoftware(sw: Software): Boolean =
    sw.certificate.level >= 3 && sw.certificate.level <= 4

  override def skin_=(s: Skin) = s match {
    case _: SkinArmored => throw new AndroidActViolation
    case s: Skin => super.skin_=(s)
    case _ => throw new AndroidActViolation
  }
}

class ConstructionWorker extends Worker {
  override def software_=(sw: Software) = sw match {
    case sw: SoftwareConstructionWorker => super.software_=(sw)
    case _ => throw new AndroidActViolation
  }
}

class ServiceEngineer extends Worker {
  override def software_=(sw: Software) = sw match {
    case sw: SoftwareServiceEngineer => super.software_=(sw)
    case _ => throw new AndroidActViolation
  }
}

class TransportWorker extends Worker {
  override def software_=(sw: Software) = sw match {
    case sw: SoftwareTransportWorker => super.software_=(sw)
    case _ => throw new AndroidActViolation
  }
}

abstract class Guard extends Android {
  protected override def validateSoftware(sw : Software): Boolean =
    sw.certificate.level == 4

  override def skin_=(s: Skin) = s match {
    case s: Skin => super.skin_=(s)
    case _ => throw new AndroidActViolation
  }
}

class PropertyGuard extends Guard {
  override def software_=(sw: Software) = sw match {
    case sw: SoftwarePropertyGuard => super.software_=(sw)
    case _ => throw new AndroidActViolation
  }
}

class BodyGuard extends Guard {
  override def software_=(sw: Software) = sw match {
    case sw: SoftwareBodyGuard => super.software_=(sw)
    case _ => throw new AndroidActViolation
  }
}

class Combatant extends Guard {
  protected override def validateSoftware(sw: Software): Boolean =
    sw.certificate.level == 5

  override def software_=(sw: Software) = sw match {
    case sw: SoftwareCombatant => super.software_=(sw)
    case _ => throw new AndroidActViolation
  }

  protected override def validateKit(a: ActorSensorKit): Boolean = true
}

abstract class Skin {
  val id: Int
}
case class SkinTouchSensitive(id: Int) extends Skin
case class SkinSolid(id: Int) extends Skin
case class SkinArmored(id: Int) extends Skin

abstract class Software {
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
