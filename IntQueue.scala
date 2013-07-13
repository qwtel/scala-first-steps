import scala.collection.mutable.ArrayBuffer

abstract class IntQueue {
  def get(): Int
  def put(x: Int)
}

class BasicIntQueue extends IntQueue {
  private val buf = new ArrayBuffer[Int]
  def get() = buf remove 0
  def put(x: Int) = buf += x
}

trait Doubling extends IntQueue {
  abstract override def put(x: Int) = super.put(2 * x)
}

trait Incrementing extends IntQueue {
  abstract override def put(x: Int) = super.put(x + 1)
}

trait Filtering extends IntQueue {
  abstract override def put(x: Int) =
    if (x >= 0) super.put(x)
}

class MyQueue extends BasicIntQueue with Doubling

object IntQueue {
  def main(args: Array[String]) = {
    val queue = new BasicIntQueue with Doubling
    queue.put(10)
    println {
      queue.get() // 20
    }

    val queue2 = new BasicIntQueue with Incrementing with Filtering

    queue2.put(-1)
    queue2.put(0)
    queue2.put(1)

    println()
    println {
      List(
        queue2.get(), // 1
        queue2.get()  // 2
      ) mkString "\n"
    }

    val queue3 = new BasicIntQueue with Filtering with Incrementing

    queue3.put(-1)
    queue3.put(0)
    queue3.put(1)

    println()
    println {
      List(
        queue3.get(), // 0
        queue3.get(), // 1
        queue3.get()  // 2
      ) mkString "\n"
    }
  }
}

