import scala.collection._

/**
 * http://jliszka.github.io/2013/10/24/exact-numeric-nth-derivatives.html
 */

abstract class Dual(val rank: Int) {
  self =>

  private val cache = mutable.HashMap[Int, Double]()

  // function of row and column to return the cell value
  protected def get(r: Int, c: Int): Double

  // Memorizing cell value accessor.
  // Since it's a diagonal-constant matrix, we can use r - c as the key.
  def apply(r: Int, c: Int): Double = cache.getOrElseUpdate(r - c, self.get(r, c))

  def +(other: Dual) = new Dual(rank) {
    def get(r: Int, c: Int) = self(r, c) + other(r, c)
  }

  def -(other: Dual) = new Dual(rank) {
    def get(r: Int, c: Int) = self(r, c) - other(r, c)
  }

  def unary_-() = new Dual(rank) {
    def get(r: Int, c: Int) = -self(r, c)
  }

  def *(other: Dual) = new Dual(rank) {
    def get(r: Int, c: Int) = (1 to rank).map(i => self(r, i) * other(i, c)).sum
  }

  def *(x: Double) = new Dual(rank) {
    def get(r: Int, c: Int) = self(r, c) * x
  }

  def /(other: Dual) = self * other.inv

  def /(x: Double) = new Dual(rank) {
    def get(r: Int, c: Int) = self(r, c) / x
  }

  lazy val I: Dual = new I(rank)

  def inv: Dual = {
    val a = self(1, 1)
    val D = self - I * a
    val N = -D / a
    List.iterate(I, rank)(_ * N).reduce(_ + _) / a
  }

  def pow(p: Int): Dual = {
    def helper(b: Dual, e: Int, acc: Dual): Dual = {
      if (e == 0) acc
      else helper(b * b, e / 2, if (e % 2 == 0) acc else acc * b)
    }
    helper(self, p, self.I)
  }

  override def toString = {
    (1 to rank).map(c => self(1, c)).mkString(" ")
  }
}

class I(rank: Int) extends Dual(rank) {
  def get(r: Int, c: Int) = if (r == c) 1 else 0
}

class E(rank: Int) extends Dual(rank) {
  def get(r: Int, c: Int) = if (r == c - 1) 1 else 0
}

object Dual {
  def main(args: Array[String]) = {
    // find the first 5 derivatives of f(x) = x^4 at f(2)
    val one = new I(6)
    val e = new E(6)
    def f(x: Dual): Dual = x * x * x * x
    println {
      f(one * 2 + e)
    }
  }
}
