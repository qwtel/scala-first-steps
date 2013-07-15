import Element.elem

sealed abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

object Express extends App {
  val f = new ExprFormatter

  val e1 =  BinOp("*",
              BinOp("/", Number(1), Number(2)),
              BinOp("+", Var("x"), Number(1)))
  val e2 =  BinOp("+",
              BinOp("/", Var("x"), Number(2)),
              BinOp("/", Number(1.5), Var("x")))

  val e3 = BinOp("/", e1, e2)

  val e4 = BinOp("/", BinOp("/", Var("a"), Var("b")), Var("c"))

  val e5 = BinOp("-", Var("a"), BinOp("-", Var("b"), Var("c")))

  def show(e: Expr) = println(f.format(e)+"\n\n")

  for (e <- Array(e1, e2, e3, e4, e5)) show (e)
}

class ExprFormatter {

  private val opGroups =
    Array(
      Set("|", "||"),
      Set("&", "&&"),
      Set("^"),
      Set("==", "!="),
      Set("<", "<=", ">", ">="),
      Set("+", "-"),
      Set("*", "%")
    )

  private val precedence = {
    val assocs =
      for {
        i <- 0 until opGroups.length
        op <- opGroups(i)
      } yield op -> i
    Map() ++ assocs
  }

  private val unaryPrecedence = opGroups.length
  private val fractionPrecedence = -1

  private def format(e: Expr, enclosingPrecedence: Int): Element = e match {
    case Var(name) => elem(name)

    case Number(num) =>
      def stripDot(s: String) =
        if (s endsWith ".0") s.substring(0, s.length - 2)
        else s
      elem(stripDot(num.toString))

    case UnOp(op, arg) =>
      elem(op)beside format(arg, unaryPrecedence)

    case BinOp("/", left, right) =>
      val top = format(left, fractionPrecedence)
      val bottom = format(right, fractionPrecedence)
      val line = elem('-', top.width max bottom.width, height = 1)
      val fraction = top above line above bottom

      if (enclosingPrecedence != fractionPrecedence) fraction // ?
      else elem(" ") beside fraction beside elem(" ")

    case BinOp(op, left, right) =>
      val opPrecedence = precedence(op)
      val l = format(left, opPrecedence)
      val r = format(right, opPrecedence + 1) // ?
      val operator = l beside elem (" " + op + " ") beside r

      if (enclosingPrecedence <= opPrecedence) operator
      else elem("(") beside operator beside elem(")")
  }

  def format(e: Expr): Element = format(e, 0)
}

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

  val capitals = Map("France" -> "Paris", "Japan" -> "Tokyo")
  // capitals get "France" // Some(Paris)
  // capitals get "North Pole" // None

  def show(x: Option[String]) = x match {
    case Some(s) => s
    case None => "?"
  }

  val expr = new BinOp("*", Number(5), Number(1))
  val BinOp(op, left, right) = expr // implicit pattern match

  // like functions, but more general
  def withDefault: Option[Int] => Int = {
    case Some(x) => x
    case None => 0
  }
}
