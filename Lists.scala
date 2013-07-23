package my {
  sealed abstract class List[+T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]

    def length: Int =  if (isEmpty) 0  else 1 + tail.length

    def drop(n: Int): List[T] =
      if (isEmpty) Nil
      else if (n <= 0) this
      else tail.drop(n - 1)

    def map[U](f: T => U): List[U] =
      if (isEmpty) Nil
      else f(head) :: tail.map(f)

    def ::[U >: T](x: U): List[U] = new ::(x, this)

    def :::[U >: T](prefix: List[U]): List[U] =
      if (prefix.isEmpty) this
      else prefix.head :: prefix.tail ::: this
  }

  case object Nil extends List[Nothing] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException("head of empty list")
    def tail: List[Nothing] = throw new NoSuchElementException("tail of empty list")
  }

  // pronounced "cons" for "construct"
  // enables pattern matching like x :: xs
  // which is the same as ::(x, xs)
  final case class ::[T](head: T, tail: List[T] = Nil) extends List[T] {
    def isEmpty: Boolean = false
  }
}
