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

  /**
   * Sorts a list by the merge sort algorithm
   *
   * @param less comparison function
   * @param list the list to be sorted
   * @tparam T the type of the list elements
   * @return the sorted list according to less
   */
  def msort[T](less: (T, T) => Boolean)(list: List[T]): List[T] = {

    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: _, y :: _) =>
          if (less(x, y)) x :: merge(xs.tail, ys)
          else y :: merge(xs, ys.tail)
      }

    val n = list.length / 2
    if (n == 0) list
    else {
      val (left, right) = list splitAt n
      merge(msort(less)(left), msort(less)(right))
    }
  }

  def msortSwapped[T](xs: List[T])(less: (T, T) => Boolean): List[T] =
    msort(less)(xs)

  /*
  val abcde = List('a', 'b', 'c', 'd', 'e')
  msort((x: Char, y: Char) => x > y)(abcde)
  msortSwapped(abcde)(_ > _)
  */

  def msortOrdered[T <: Ordered[T]](xs: List[T]): List[T] =
    msortSwapped(xs)(_ < _)
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

  // O(n^2)
  def reverse[T](xs: List[T]): List[T] =
    xs match {
      case Nil => Nil
      case x :: _ => reverse(xs.tail) ::: List(x)
    }

  // O(n)
  // def reverseLeft[T](xs: List[T]): List[T] = (List[T]() /: xs)((ys, y) => y :: ys)
  def reverseLeft[T](xs: List[T]): List[T] = xs.foldLeft(List[T]())((ys, y) => y :: ys)

  // Slow
  def flattenLeft[T](xss: List[List[T]]) = xss.foldLeft(List[T]())(_ ::: _)

  // Faster
  def flattenRight[T](xss: List[List[T]]) = xss.foldRight(List[T]())(_ ::: _)
}
