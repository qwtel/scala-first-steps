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

    def map[U](f: T => U): List[U] = {
      def mapHelper(list: List[T], res: List[U]): List[U] =
        list match {
          case x :: xs => mapHelper(xs, f(x) :: res)
          case Nil => res
        }
      mapHelper(this, Nil).reverse
    }

    def reverse: List[T] = this.foldLeft(List[T]())((ys, y) => y :: ys)

    private def foldLeft[B](zero: B)(f: (B, T) => B): B =
      this match {
        case x :: xs => xs.foldLeft(f(zero, x))(f)
        case Nil => zero
      }

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

  object List {
    def apply[T](): List[T] = Nil
  }

  object PerformanceTest extends App {
    def time[A](f: => A) = {
      val s = System.nanoTime
      val ret = f
      println("time: "+(System.nanoTime-s)/1e9+"s")
      ret
    }

    //val x = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: Nil
    //val y = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: scala.collection.immutable.Nil

    var x = List[Int]()
    (1 to 1000000).foreach((i) => x = i :: x)
    time {
      x.map(_ + 1)
      x.map(_ * 2)
      x.map(_ / 2)
      x.map(_ - 1)
      x.map(_.toString)
    }

    var y = scala.collection.immutable.List[Int]()
    (1 to 1000000).foreach((i) => y = i :: y)
    time {
      y.map(_ + 1)
      y.map(_ * 2)
      y.map(_ / 2)
      y.map(_ - 1)
      y.map(_.toString)
    }
  }
}
