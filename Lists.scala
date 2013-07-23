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

    // these to methods are necessary to make the x :: xs syntax possible
    // Anything here is implicitly converted to a list
    // then it has a ::-method that returns a new list construct
    // methods that end with : are applied in reverse order, e.g. x :: xs == xs.::(x)
    // the type parameter [S >: T] is to support covariant lists, e.g. List[Cat] <: List[Animal]
    implicit def asList[T](a: T): List[T] = new ::(a, Nil)
    def ::[S >: T](x: S): List[S] = new ::(x, this)
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

/*
::(1)
::(1, Nil)
::(1, 2)
::(1, asList(2))
::(1, ::(2))
3.::(asList(1), 2)
2.::(asList(1))

1 :: 2 // a lot of weirdness going on to make this possible
*/
