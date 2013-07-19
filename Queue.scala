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

trait Queue[+T] {
  def head: T
  def tail: Queue[T]
  def append[U >: T](x: U): Queue[U]
}

object Queue {
  def apply[T](xs: T*): Queue[T] = new QueueImpl[T](xs.toList, Nil)

  private class QueueImpl[+T] (
    private[this] var leading: List[T],
    private[this] var trailing: List[T]
  ) extends Queue[T] {

    def mirror() =
      if (leading.isEmpty) {
        while(!trailing.isEmpty) {
          leading = trailing.head :: leading
          trailing = trailing.tail
        }
      }

    def head = {
      mirror()
      leading.head
    }

    def tail = {
      mirror()
      new QueueImpl(leading.tail, trailing)
    }

    def append[U >: T](x: U) =
      new QueueImpl[U](leading, x :: trailing)
  }
}
