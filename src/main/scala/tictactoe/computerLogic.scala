package tictactoe

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

trait hasRow :
  def rowToReplace(row : Int, urgent : Boolean) : Unit

object row:
  var isMyTurn : Boolean = false
  val possibleRows : ArrayBuffer[Any] = ArrayBuffer[Any]()
  var num : Int = 0
  val sums: ArrayBuffer[Int] = ArrayBuffer[Int]()
  val newBoard : Array[Array[Int]] = Array.ofDim[Int](3,3)
class computerLogic(computerBoard : Array[Array[Int]]) extends hasRow:
  def removeTwos() : Unit = {
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        if computerBoard(i)(j) == 2 then row.newBoard(i)(j) = 5
        else if computerBoard(i)(j) == 1 then row.newBoard(i)(j) = 1
      }
    }
  }
  def getSum() : Unit = {
    for (i <- 0 until 3) do
      val j = computerBoard(i).sum
      row.sums += j
  }

  def placementLogic() : Unit = {
    row.isMyTurn = true
    for elem <- row.sums do
      elem match
        case 10 =>
          rowToReplace(row.num, false) //only two fives could satisfy this condition, if it's comps turn, then finish the row
        case 6 => //only option is a 5 and one 1 in the row, not too urgent
          rowToReplace(row.num,false)
        case 5 => // only option is one 5 in the row, aka player 2 has only placed one 2 in this row
          rowToReplace(row.num, false)
        case 2 =>
          rowToReplace(row.num, true) //always a pair of ones, stop it in it's tracks, even if it means passing an opportunity to win
        case 1 => println("1") //player 1 has only played once in this row, not too urgent
        case 0 => println("0")
        case _ => println("greater than 8")
      row.num += 1
  }
  override def rowToReplace(rower: Int, urgent : Boolean): Unit = {
    for (elem <- 0 until 3) do
      if board.bard(rower)(elem) != 1 then
        if row.sums(elem) < 7 then
          if row.isMyTurn then
            row.isMyTurn = false
            board.bard(rower)(elem) = 2
            round.nextRound
        else if row.sums(elem) >= 7 then
            println()
      if row.sums(elem) == 10 then
        if row.isMyTurn then
          if board.bard(rower)(elem) != 2 then
            row.isMyTurn = false
            board.bard(rower)(elem) = 2
      if row.sums(elem) == 5 then
        if row.isMyTurn then
          if row.num <= 2 then
              if board.bard(rower-1)(elem) == 2 || board.bard(rower+1)(elem) == 2 then
                board.bard(rower)(elem) == 2
                row.isMyTurn = false
          if row.num == 3 then // check for vertical win possibilities, and stop them
            if board.bard(rower-1)(elem) == 1 && board.bard(rower-2)(elem) == 1 then
              if board.bard(rower)(elem) != 2 || board.bard(rower)(elem) != 1 then
                if row.isMyTurn then
                  board.bard(rower)(elem) = 2
                  row.isMyTurn = false
      if board.bard(2)(elem) == 1 && board.bard(0)(elem) == 1 then
        if row.isMyTurn then
          board.bard(1)(elem) = 2
          row.isMyTurn = false
  }
