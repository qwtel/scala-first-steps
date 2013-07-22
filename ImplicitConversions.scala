object ImplicitConversions extends App {
  import java.awt.event.{ActionEvent, ActionListener}
  import javax.swing.JButton

  implicit def function2ActionListener(f: ActionEvent => Unit) =
    new ActionListener {
      def actionPerformed(e: ActionEvent) = f(e)
    }

  val button = new JButton
  button.addActionListener(
    (_: ActionEvent) => println("pressed!")
  )

  implicit def boolean2MyBoolean(b: Boolean) = new MyBoolean(b)

  class MyBoolean(b: Boolean) {
    def and(o: Boolean) = o && b
    def or(o: Boolean) = o || b
    def is(o: Boolean) = o == b
    def isnt(o: Boolean) = o != b
  }

  implicit def addDoIt(a: Any): Any { def doIt() } = {
    new AnyRef {
      def doIt() = println("Did it!")
    }
  }

  3.doIt()
  "four".doIt()
  List().doIt()
  button.doIt()
  //null.doIt()
}
