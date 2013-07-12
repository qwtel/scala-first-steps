import Element.elem

abstract class Element {
  def contents: Array[String]
  def height = contents.length
  def width = if (height == 0) 0 else contents(0).length

  def above(that: Element) = elem(this.contents ++ that.contents)

  def beside(that: Element) = {
    elem(
      for {
        (line1, line2) <- this.contents zip that.contents
      } yield line1 + line2
    )
  }

  final override def toString = contents.mkString("\n")
}

object Element {
  private class ArrayElement(val contents: Array[String]) extends Element

  private class LineElement(s: String) extends Element {
    val contents = Array(s)
    override val width = s.length
    override val height = 1
  }

  private class UniformElement(
    ch: Char,
    override val width: Int,
    override val height: Int
  ) extends Element {
    private val line = ch.toString * width
    def contents = (for (_ <- 1 to height) yield line).toArray
  }

  def elem(contents: Array[String]): Element =
    new ArrayElement(contents)

  def elem(chr: Char, width: Int, height: Int): Element =
    new UniformElement(chr, width, height)

  def elem(line: String): Element =
    new LineElement(line)
}
