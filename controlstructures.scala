import java.io.{PrintWriter, File}

// abstractions of a control structures
def twice(op: Double => Double, x: Double) = op(op(x))
twice(_ + 1, 5)

// open a resource, operate on it, then close the resource
def withPrintWriter(file: File)(op: PrintWriter => Unit) = {
  val writer = new PrintWriter(file)
  try {
    op(writer)
  } finally {
    writer.close()
  }
}

val file = new File("data.txt")

// using currying, looks like a "native" control structure!
withPrintWriter(file) {
  writer => writer.println(new java.util.Date)
}

// How it works:
println("Hello world!")

// Can use curly braces for single argument
println { "Hello world!" }

// but only for single arguments:
val g = "Hello, World!"
// g.substring { 7, 9 } // <- error
g.substring(7, 9) // works

// Therefore: using currying so the last argument is in its own argument list
// The first argument in parenthesis, the second in curly braces
