abstract class Currency {
  val amount: Long
  def designation: String
  override def toString = amount + " " + designation
  def +(that: Currency): Currency = ???
  def *(that: Currency): Currency = ???
}

abstract class Dollar extends Currency {
  override def designation = "USD"
}
