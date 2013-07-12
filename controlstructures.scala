import java.io.{PrintWriter, File}

// abstractions of a control structures
def twice(op: Double => Double, x: Double) = op(op(x))
twice(_ + 1, 5)

// open a resource, operate on it, then close the resource
def withPrintWriter(file: File, op: PrintWriter => Unit) = {
  val writer = new PrintWriter(file)
  try {
    op(writer)
  } finally {
    writer.close()
  }
}

// aka "loan pattern"
withPrintWriter(
  new File("date.txt"),
  writer => writer.println(new java.util.Date)
)

