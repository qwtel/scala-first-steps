class Food
class Grass extends Food

abstract class Animal {
  type SuitableFood <: Food
  def eat(food: SuitableFood) = "Eating food"

  def encounter(a: Animal) = "Generic animal encounter"
}

class Cow extends Animal {
  type SuitableFood = Grass
  override def eat(food: Grass) = "Eating grass"
}

class Bunny extends Animal {
  override def encounter(a: Animal) = a match {
    case _: Bunny => "Mate"
    case _: Lion => "Run away"
    case _ => "Generic Bunny encounter"
  }
}

class Lion extends Animal {
  override def encounter(a: Animal) = a match {
    case _: Bunny => "Eat"
    case _: Lion => "Fight"
    case _ => "Generic Lion encounter"
  }
}

object Animal extends App {
  def printEncounter(a1: Animal, a2: Animal) =
    println {
      a1 encounter a2
    }

  val bunny = new Bunny
  val lion = new Lion

  bunny encounter lion // Run away
  bunny encounter bunny // Mate
  lion encounter lion // Fight
  lion encounter bunny // Eat

  printEncounter(bunny, lion) // Run away
  printEncounter(bunny, bunny) // Mate
  printEncounter(lion, lion) // Fight
  printEncounter(lion, bunny) // Eat
}
