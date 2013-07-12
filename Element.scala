abstract class Element {
  def contents: Array[String]
  def height = contents.length
  def width = if (height == 0) 0 else contents(0).length
  final override def toString = contents.mkString("\n")
}

class ArrayElement(val contents: Array[String]) extends Element

class LineElement(s: String) extends Element {
  val contents = Array(s)
  override val width = s.length
  override val height = 1
}

class UniformElement(
  ch: Char,
  override val width: Int,
  override val height: Int
) extends Element {
  private val line = ch.toString * width
  def contents = (for (i <- 1 to height) yield line).toArray
}
