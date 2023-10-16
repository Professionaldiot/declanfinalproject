package tictactoe

import scala.swing.*

def GUI() : Unit = {
  new Frame() {
    title = "MAIN SCREEN"
    preferredSize = new Dimension(500,500)
    contents = new GridPanel(5,5) {
      contents += new ToggleButton("TIC-TAC-TOE") {
        reactions += {
          case event.ButtonClicked(_) =>
            close()
            amComp()
        }
      }
      contents += new ToggleButton("HANG-MAN") {
        reactions += {
          case event.ButtonClicked(_) =>
            close()
            compOrNot()
        }
      }
    }
    pack()
    centerOnScreen()
    open()
  }
}