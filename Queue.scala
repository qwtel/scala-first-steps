class SlowAppendQueue[T](elems: List[T]) {
  def head = elems.head
  def tail = new SlowAppendQueue(elems.tail)
  def append(x: T) = new SlowAppendQueue(elems ::: List(x))
}

class SlowHeadQueue[T](smele: List[T]) {
  def head = smele.last
  def tail = new SlowHeadQueue(smele.init)
  def append(x: T) = new SlowHeadQueue(x :: smele)
}

trait Queue[T] {
  def head: T
  def tail: Queue[T]
  def append(x: T): Queue[T]
}

object Queue {
  def apply[T](xs: T*) = new QueueImpl[T](xs.toList, Nil)

  private class QueueImpl[T] (
    private val leading: List[T],
    private val trailing: List[T]
  ) extends Queue[T] {

    private def mirror =
      if (leading.isEmpty)
        new QueueImpl(trailing.reverse, Nil)
      else
        this

    def head = mirror.leading.head

    def tail = {
      val q = mirror
      new QueueImpl(q.leading.tail, q.trailing)
    }

    def append(x: T) =
      new QueueImpl(leading, x :: trailing)
  }

}