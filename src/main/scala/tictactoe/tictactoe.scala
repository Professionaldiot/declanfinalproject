package tictactoe

import scala.language.postfixOps
import scala.swing.*

trait boardState :
  def currentState(board : Array[Array[Int]]) : Unit

object board :
  var bard : Array[Array[Int]] = Array.ofDim[Int](3,3)

class boardAdder extends boardState {
  def bored(player : Int, row : Int, colm : Int): Unit = {
    board.bard(row)(colm) = player
  }

  override def currentState(args: Array[Array[Int]]): Unit = {
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        print(args(i)(j) + " ")
      }
      println()
    }
  }
}
//DONEa: make the x's and o's
object ourBoard:
  var hoard: Array[Array[Char]] = Array.ofDim[Char](3, 3)

def theBirdsAndBees() : Unit = {
  for (row <- 0 until 3) {
    for (colm <- 0 until 3) {
      if board.bard(row)(colm) != 0 then
        if board.bard(row)(colm) == 1 then ourBoard.hoard(row)(colm) = 'X'
        if board.bard(row)(colm) == 2 then ourBoard.hoard(row)(colm) = 'O'
    }
  }
}

def makeStringFromArrayAny(row : Int): Array[Array[Char]] = {
  for (i <- 0 until 3) {
    print(ourBoard.hoard(row)(i).toString + " " )
  }
  ourBoard.hoard
}

def newButton() : Unit = {
  val player = new boardAdder()
  if round.againstComputer then
    round.roundNum += 1
    addToArray()
  theBirdsAndBees()
  new Frame() {
    title = "TIC-TAC-TOE"
    preferredSize = new Dimension(500,500)
    contents = new GridPanel(8,5) {
      contents += new Label("Tic")
      contents += new Label("Tac")
      contents += new Label("Toe")
      for (i <- 0 until 3) {for (j <- 0 until 3) {contents += new TextArea(f"\u0009${ourBoard.hoard(i)(j).toString} | ")}}

      contents += new ToggleButton("1") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            close()
            player.bored(round.player, 0, 0)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("4") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            close()
            player.bored(round.player, 0,1)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("7") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            close()
            player.bored(round.player, 0,2)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("2") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            close()
            player.bored(round.player, 1, 0)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("5") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            close()
            player.bored(round.player, 1, 1)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("8") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            close()
            player.bored(round.player, 1,2)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("3") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            close()
            player.bored(round.player, 2, 0)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("6") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            close()
            player.bored(round.player, 2,1)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("9") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            close()
            player.bored(round.player, 2, 2)
            player.currentState(board.bard)
            win.bundle
            enabled = false
            println()
        }
      }
      contents += new ToggleButton("Reset") {
        reactions += {
          case event.ButtonClicked(enabled_) =>
            println("Resetting...")
            for (i <- 0 until 3) {
              for (j <- 0 until 3) {
                board.bard(i)(j) = 0
              }
            }
            row.newBoard = Array.ofDim[Int](3,3)
            ourBoard.hoard = Array.ofDim[Char](3, 3)
            round.player = 1
            round.tieCounter = 0
            round.roundNum = 0
            close()
            main()
        }
      }
      contents += new ToggleButton("Back to main GUI") {
        reactions += {
          case event.ButtonClicked(_) =>
            close()
            GUI()
        }
      }
    }
    pack()
    centerOnScreen()
    open()
  }
}

def amComp() : Unit = {
  new Frame {
    title = "Computer or player"
    preferredSize = new Dimension(500,500)
    contents = new GridPanel(3,3) {
      contents += new ToggleButton("Against Computer") {
        reactions += {
          case event.ButtonClicked(_) =>
            close()
            round.againstComputer = true
            newButton()
        }
      }
      contents += new ToggleButton("Against another player") {
        reactions += {
          case event.ButtonClicked(_) =>
            close()
            round.againstComputer = false
            newButton()
        }
      }
    }
    pack()
    centerOnScreen()
    open()
  }
}

object win :
  def bundle = {
    round.nextRound
    winner.horiz(board.bard)
    winner.vertical(board.bard)
    winner.diagonal(board.bard)
    if !round.againstComputer then
      newButton()
      if round.tieChecker then println("There is a tie, no one won.")
  }

object winner :
  private var i = 0
  def horiz(board : Array[Array[Int]]): Unit = {
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        this.i += 1
        if this.i == 3 then
          this.i = 0
          if board(i)(j) == 0 then
            print(" ")
          else if board(i)(j) == board(i)(j-2) && board(i)(j) == board(i)(j-1) then
            println()
            if !round.againstComputer then
              round.nextRound
            println("player " + round.player + " has won")
          if !round.againstComputer then
            if round.roundNum == 9 then
              if !(board(i)(j) == board(i)(j-2) && board(i)(j) == board(i)(j-1)) then
                round.tieCounter += 1
            end if
          else if round.againstComputer then
            if round.roundNum >= 10 then
              if !(board(i)(j) == board(i)(j - 2) && board(i)(j) == board(i)(j - 1)) then
                round.tieCounter += 1
      }
    }
  }

  def vertical(board: Array[Array[Int]]): Unit = {
    for (i <- 0 until 1) {
      for (j <- 0 until 3) {
        if board(i)(j) == 0 then
          print(" ")
        else if board(i)(j) == board(i + 2)(j) && board(i)(j) == board(i + 1)(j) then
          println()
          if !round.againstComputer then
            round.nextRound
          println(board(i)(j) + " ")
          println("player " + round.player + " has won")
        if !round.againstComputer then
          if round.roundNum == 9 then
            if !(board(i)(j) == board(i + 2)(j) && board(i)(j) == board(i + 1)(j)) then
              round.tieCounter += 1
          end if
        else if round.againstComputer then
            if round.roundNum >= 10 then
              if !(board(i)(j) == board(i + 2)(j) && board(i)(j) == board(i + 1)(j)) then
                round.tieCounter += 1
      }
    }
  }

  def diagonal(board : Array[Array[Int]]): Unit = {
    if board(0)(0) == board(1)(1) && board(0)(0) == board(2)(2) then
      if board(0)(0) == 0 then
        print(" ")
      else
        println()
        if !round.againstComputer then
          round.nextRound
        println("player " + round.player + " has won")
    else if board(0)(2) == board(1)(1) && board(0)(2) == board(2)(0) then
      if board(0)(2) == 0 then
        print(" ")
      else
        println()
        if !round.againstComputer then
          round.nextRound
        println("player " + round.player + " has won")
    if !round.againstComputer then
      if round.roundNum == 9 then
        if !(board(0)(0) == board(1)(1) && board(0)(0) == board(2)(2)) || !(board(0)(2) == board(1)(1) && board(0)(2) == board(2)(0)) then
          round.tieCounter += 1
      end if
    else if round.againstComputer then
        if round.roundNum >= 10 then
          if !(board(0)(0) == board(1)(1) && board(0)(0) == board(2)(2)) || !(board(0)(2) == board(1)(1) && board(0)(2) == board(2)(0)) then
            round.tieCounter += 1
  }

object round :
  var player : Int = 1
  var roundNum : Int = 0
  var tieCounter : Int = 0
  var againstComputer = false

  def nextRound = {
    if againstComputer then
      roundNum += 1
      row.isMyTurn = true
      addToArray()
      theBirdsAndBees()
      newButton()
    else if !againstComputer then
      if player == 1 then
        roundNum += 1
        player = 2
      else if player == 2 then
        roundNum += 1
        player = 1
      else
        player = 1
  }

  def check(player : Int) : Int = {
    this.player
  }

  def tieChecker  : Boolean = {
    if roundNum == 9 then
      if tieCounter >= 0 then true
      else false
    else false
  }


def main() : Unit = {
  amComp()
}