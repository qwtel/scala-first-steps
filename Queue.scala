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

class Queue[T] private (
  private val leading: List[T],
  private val trailing: List[T]
) {
  def this() = this(Nil, Nil)

  def this(elems: T*) = this(elems.toList, Nil)

  private def mirror =
    if (leading.isEmpty)
      new Queue(trailing.reverse, Nil)
    else
      this

  def head = mirror.leading.head

  def tail = {
    val q = mirror
    new Queue(q.leading.tail, q.trailing)
  }

  def append(x: T) =
    new Queue(leading, x :: trailing)
}