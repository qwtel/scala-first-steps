class Rational(n: Int, d: Int) extends Ordered[Rational] {

  require(d != 0)

  private val g = gcd(n.abs, d.abs)
  val numer = n / g
  val denom = d / g

  def this(n: Int) = this(n, 1)

  def +(that: Rational) = add(that)
  def add(that: Rational) =
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom
    )
  def +(that: Int): Rational = add(that)
  def add(that: Int): Rational = this + new Rational(that)

  def -(that: Rational) = sub(that)
  def sub(that: Rational) =
    new Rational(
      this.numer * that.denom - that.numer * this.denom,
      this.denom * that.denom
    )
  def -(that: Int): Rational = sub(that)
  def sub(that: Int): Rational = this - new Rational(that)

  def *(that: Rational) = mul(that)
  def mul(that: Rational) =
    new Rational(
      this.numer * that.numer,
      this.denom * that.denom
    )
  def *(that: Int): Rational = mul(that)
  def mul(that: Int): Rational = this * new Rational(that)

  def /(that: Rational) = mul(that)
  def div(that: Rational) =
    new Rational(
      this.numer * that.denom,
      this.denom * that.numer
    )
  def /(that: Int): Rational = div(that)
  def div(that: Int): Rational = this / new Rational(that)

  override def toString = numer+"/"+denom

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def compare(that: Rational) =
    (this.numer * that.denom) - (that.numer * this.denom)
}

object Rational {
  implicit def intToRational(x: Int) = new Rational(x)
}
