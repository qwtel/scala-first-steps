sealed abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

object Expr {
  def simplifyTop(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => e
    case BinOp("+", e, Number(0)) => e
    case BinOp("*", e, Number(1)) => e
    case _ => expr
  }

  def generalSize(x: Any) = x match {
    case s: String => s.length
    case m: Map[_, _] => m.size
    case _ => -1
  }

  def isIntIntMap(x: Any) = x match {
    case m: Map[Int, Int] => true // type gets erased, not possible
    case _ => false
  }

  def isStringArray(x: Any) = x match {
    case a: Array[String] => "yes"
    case _ => "no"
  }

  def simplifyDoubleAbs(expr: Expr): Expr = expr match {
    case UnOp("abs", e @ UnOp("abs", _)) => e // UnOp bound to e
    case _ => expr
  }

  def simplifyAdd(e: Expr) = e match {
    case BinOp("+", x, y) if x == y =>
      BinOp("*", x, Number(2)) // error: double binding to x
    case _ => e
  }

  def simplifyAll(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => simplifyAll(e)
    case BinOp("+", e, Number(0)) => simplifyAll(e)
    case BinOp("*", e, Number(1)) => simplifyAll(e)
    case UnOp(op, e) => UnOp(op, simplifyAll(e))
    case BinOp(op, l, r) => BinOp(op, simplifyAll(l), simplifyAll(r))
    case _ => expr
  }

  def describe(e: Expr): String = (e: @unchecked) match {
    case Number(_) => "a number"
    case Var(_) => "a variable"
  }
}
