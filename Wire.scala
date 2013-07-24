package simulation {
  class Simulation {
    type Action = () => Unit

    case class WorkItem(time: Int, action: Action)

    private var curtime = 0
    def currentTime = curtime

    private var agenda: List[WorkItem] = List()

    private def insert(ag: List[WorkItem], item: WorkItem): List[WorkItem] = {
      if (ag.isEmpty || item.time < ag.head.time) item :: ag
      else ag.head :: insert(ag.tail, item)
    }

    def afterDelay(delay: Int)(block: => Unit) {
      val item = WorkItem(currentTime + delay, () => block)
      agenda = insert(agenda, item)
    }

    def run() {
      afterDelay(0) {
        println("*** simulation started, time = " + currentTime + " ***")
      }
      while(!agenda.isEmpty) next()
    }

    def next() {
      (agenda: @unchecked) match {
        case item :: rest =>
          agenda = rest
          curtime =item.time
          item.action()
      }
    }
  }
}

/*
class Wire
val a, b, c = new Wire

def inverter(input: Wire, output: Wire)
def andGate(a1: Wire, a2: Wire, output: Wire)
def orGate(o1: Wire, o2: Wire, output: Wire)

// produces s = (a + b) % 2 and a carry c = (a + b) / 2
def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) {
  val d, e = new Wire
  orGate(a, b, d)
  andGate(a, b, c)
  inverter(c, e)
  andGate(d, e, s)
}

def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire) {
  val s, c1, c2 = new Wire
  halfAdder(a, cin, s, c1)
  halfAdder(b, s, sum, c2)
  orGate(c1, c2, cout)
}
*/