import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton

val button = new JButton
button.addActionListener(
  new ActionListener {
    def actionPerformed(e: ActionEvent) {
      println("pressed!")
    }
  }
)