class ScalaCSVTokenizer(text: String) extends java.util.Enumeration[Object] {
  import ScalaCSVTokenizer._

  private type Word = List[Char]

  private var tokens = outsideEscape(text.toList).reverse
    .map(word => word.reverse.mkString)

  private def outsideEscape(text: List[Char], words: List[Word] = Nil, word: Word = Nil): List[Word] =
    text match {
      case Delimiter :: cs => outsideEscape(cs, word :: words, Nil)
      case Escape :: cs => insideEscape(cs, words, word)
      case c :: cs => outsideEscape(cs, words, c :: word)
      case Nil => word :: words
    }

  private def insideEscape(text: List[Char], words: List[Word] = Nil, word: Word = Nil): List[Word] =
    text match {
      case Escape :: Escape :: cs => insideEscape(cs, words, Escape :: word)
      case Escape :: cs  => outsideEscape(cs, words, word)
      case c :: cs => insideEscape(cs, words, c :: word)
      case Nil => word :: words // throw new Exception("No closing \" found")
    }

  override def hasMoreElements = this.hasMoreTokens
  def hasMoreTokens = !tokens.isEmpty

  override def nextElement(): Object = this.nextToken()
  def nextToken(): String = {
    if (hasMoreTokens) {
      val res = tokens.head
      tokens = tokens.tail
      res
    }
    else throw new NoSuchElementException()
  }

  def countTokens: Int = tokens.length
}

object ScalaCSVTokenizer {
  private val Escape = '"'
  private val Delimiter = ';'
}

