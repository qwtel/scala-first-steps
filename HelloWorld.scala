import scala.io.Source

val file = if (!args.isEmpty) args(0) else "HelloWorld.scala"
val lines = Source.fromFile(file).getLines.toList

def widthOfLength(s: String) = s.length.toString.length

val longestLine = lines.reduceLeft(
  (a, b) => if (a.length > b.length) a else b
)

val maxWidth = widthOfLength(longestLine)

for (line <- lines) {
  val numSpaces = maxWidth - widthOfLength(line)
  val padding = " " * numSpaces
  println(padding + line.length + " | " + line)
}
