object Sort {
  def isort(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case x :: xs1 => insert(x, isort(xs1))
  }

  def insert(x: Int, xs: List[Int]): List[Int] =
    xs match {
      case Nil => List(x)
      case y :: _ =>
        if (x <= y) x :: xs
        else y :: insert(x, xs.tail)
  }
}

object ListMethods {
  def append[T](xs: List[T], ys: List[T]): List[T] =
    xs match {
      case Nil => ys
      case x :: _ => x :: append(xs.tail, ys)
    }

  def length[T](xs: List[T]): Int = {
    def helper(length: Int, xs: List[T]): Int =
      xs match {
        case Nil => length
        case x :: _ => helper(length + 1, xs.tail)
      }
    helper(0, xs)
  }

  def reverse[T](xs: List[T]): List[T] =
    xs match {
      case Nil => Nil
      case x :: _ => reverse(xs.tail) ::: List(x)
    }
}
