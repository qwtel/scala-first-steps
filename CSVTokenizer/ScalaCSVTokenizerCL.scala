import scala.io.Source

object ScalaCSVTokenizerCL {
  def main(args: Array[String]) = {
    if (args.length == 1) {
      val lines = Source.fromFile(args(0)).getLines.toList
      for (line <- lines) {
        val tokenizer = new ScalaCSVTokenizer(line)
        while (tokenizer.hasMoreTokens) {
          println(tokenizer.nextToken())
        }
      }
    }
    else {
      println("Tokenize: scala ScalaCSVTokenizerCL test.csv")
    }
  }
}
