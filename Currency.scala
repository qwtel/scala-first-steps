abstract class AbstractCurrency {
  type Currency <: AbstractCurrency
  val amount: Long
  def designation: String
  override def toString = amount + " " + designation
  def +(that: Currency): Currency = ???
  def *(that: Currency): Currency = ???
}

abstract class Dollar extends AbstractCurrency {
  type Currency = Dollar
  def designation = "USD"
}
