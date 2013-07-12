import ChecksumAccumulator.calculate

object FallWinterSpringSummer extends App {
  for (seasion <- List("fall", "winter", "spring"))
    println(seasion + ": " + calculate(seasion))
}
