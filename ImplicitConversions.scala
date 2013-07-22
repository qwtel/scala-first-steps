import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton

implicit def function2ActionListener(f: ActionEvent => Unit) =
  new ActionListener {
    def actionPerformed(e: ActionEvent) = f(e)
  }

val button = new JButton
button.addActionListener(
  function2ActionListener(
    (_: ActionEvent) => println("pressed!")
  )
)
