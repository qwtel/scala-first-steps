import Element.elem

abstract class Element {
  def contents: Array[String]
  def height = contents.length
  def width = if (height == 0) 0 else contents(0).length

  def above(that: Element) = {
    val thisSyncedWidth = this widen that.width
    val thatSyncedWidth = that widen this.width
    elem(thisSyncedWidth.contents ++ thatSyncedWidth.contents)
  }

  def beside(that: Element) = {
    val thisSyncedHeight = this heighten that.height
    val thatSyncedHeight = that heighten this.height
    elem(
      for {
        (line1, line2) <- thisSyncedHeight.contents zip thatSyncedHeight.contents
      } yield line1 + line2
    )
  }

  private def widen(w: Int): Element =
    if (w <= width) this
    else {
      val left = elem(' ', (w - width) / 2, height)
      val right = elem(' ', w - width - left.width, height)
      left beside this beside right
    }

  private def heighten(h: Int): Element =
    if (h <= height) this
    else {
      val top = elem(' ', width, (h - height)/2)
      val bottom = elem(' ', width, h - height - top.height)
      top above this above bottom
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
