abstract class Animal {
  def encounter(a: Animal) = "Generic animal encounter"
}

class Bunny extends Animal {
  override def encounter(a: Animal) = "Generic Bunny encounter"
  def encounter(b: Bunny) = "Mate"
  def encounter(l: Lion) = "Run away"
}

class Lion extends Animal {
  override def encounter(a: Animal) = "Generic Lion encounter"
  def encounter(b: Bunny) = "Eat"
  def encounter(l: Lion) = "Fight"
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

  printEncounter(bunny, lion) // Generic Bunny encounter
  printEncounter(bunny, bunny) // Generic Bunny encounter
  printEncounter(lion, lion) // Generic Lion encounter
  printEncounter(lion, bunny) // Generic Lion encounter
}
