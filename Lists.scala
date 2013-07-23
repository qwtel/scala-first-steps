sealed abstract class MyList[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: MyList[T]
}

case object Nil extends MyList[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("head of empty list")
  def tail: MyList[Nothing] = throw new NoSuchElementException("tail of empty list")
}

// pronounced "cons" for "construct"
// enables pattern matching like x :: xs
// which is the same as ::(x, xs)
final case class ::[T](head: T, tail: MyList[T]) extends MyList[T] {
  def isEmpty: Boolean = false
}
