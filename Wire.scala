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

  abstract class BasicCircuitSimulation extends Simulation {
    val InverterDelay: Int
    val AndGateDelay: Int
    val OrGateDelay: Int

    class Wire {
      private var sigVal = false
      private var actions: List[Action] = List()

      def getSignal = sigVal

      def setSignal(s: Boolean) =
        if (s != sigVal) {
          sigVal = s
          actions foreach (action => action())
        }

      def addAction(action: Action) = {
        actions = action :: actions
        action()
      }
    }

    def inverter(input: Wire, output: Wire) = {
      def invertAction() {
        val inputSig = input.getSignal
        afterDelay(InverterDelay) {
          output setSignal !inputSig
        }
      }
      input addAction invertAction
    }

    def andGate(a1: Wire, a2: Wire, output: Wire) = {
      def andAction() = {
        val a1Sig = a1.getSignal
        val a2Sig = a2.getSignal
        afterDelay(AndGateDelay) {
          output setSignal (a1Sig & a2Sig)
        }
      }
      a1 addAction andAction
      a2 addAction andAction
    }

    def orGate(o1: Wire, o2: Wire, output: Wire) = {
      def orAction() = {
        val o1Sig = o1.getSignal
        val o2Sig = o2.getSignal
        afterDelay(AndGateDelay) {
          output setSignal (o1Sig | o2Sig)
        }
      }
      o1 addAction orAction
      o2 addAction orAction
    }

    def probe(name: String, wire: Wire) {
      def probeAction() {
        println(name + " " + currentTime + " nav-value = " + wire.getSignal)
      }
      wire addAction probeAction
    }
  }
}

/*
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