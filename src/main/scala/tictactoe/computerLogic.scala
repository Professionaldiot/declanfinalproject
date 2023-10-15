package tictactoe

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

trait hasRow :
  def rowToReplace(row : Int) : Unit

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
    for (elem <- 0 until 2) do
      rowToReplace(elem)
      row.num+=1
  }
  override def rowToReplace(rower: Int): Unit = {
    for (elem <- 0 until 2) do
      if row.sums(elem) == 0 then
        if elem <= 2 then
          if row.sums(elem+1) == 7 || row.sums(elem+1) == 11 then //there is at least one full row below our current row
            if elem == 1 then
              if row.isMyTurn then
                if (row.sums(elem-1) == 7 || row.sums(elem-1) == 11) && (row.sums(elem+1) == 7 || row.sums(elem+1) == 11) then
                  board.bard(1)(1) = 2
                  row.isMyTurn = false
              end if
        else if elem == 3 then //check if both rows above it are full
            if (row.sums(elem-1) == 7 || row.sums(elem-1) == 11) && (row.sums(elem-2) == 7 || row.sums(elem-2) == 11) then
              if row.isMyTurn then
                board.bard(rower)(1) = 2//will only happen if both rows are full
            else
              println()
        end if
      else //if the row is 0 or not 0 check the other rows, if it is 0, use a recursive call to achieve this
        if board.bard(rower)(elem) != 1 && board.bard(rower)(elem) != 2 then // this will place a two in the row where it isn't ones
          if row.sums(elem) < 7  then
            if row.isMyTurn then
                row.isMyTurn = false
                board.bard(rower)(elem) = 2
          if row.sums(elem) >= 7 then
            if elem <= 2 then if row.sums(elem+1) == 1 then
              if row.isMyTurn then
                if board.bard(2)(elem) == 1 && board.bard(0)(elem) == 1 then
                  board.bard(1)(elem) = 2
        if row.sums(elem) == 10 then //this is where there are two 2's in the row
          if row.isMyTurn then
            if board.bard(rower)(elem) != 2 then
              row.isMyTurn = false
              board.bard(rower)(elem) = 2
        if row.sums(elem) == 5 then //this is where only one 2 is in the row
          if row.isMyTurn then
            if row.num == 1 then
              if board.bard(1)(elem) == 2 && board.bard(2)(elem) == 2 then
                row.isMyTurn = false
                board.bard(0)(elem) = 2
            if row.num == 2 then
                if board.bard(rower-1)(elem) == 2 && board.bard(rower+1)(elem) == 2 then
                  board.bard(rower)(elem) == 2
                  row.isMyTurn = false
            if row.num == 3 then // check for vertical win possibilities, and stop them
              if board.bard(rower-1)(elem) == 1 && board.bard(rower-2)(elem) == 1 then
                if board.bard(rower)(elem) != 2 || board.bard(rower)(elem) != 1 then
                  if row.isMyTurn then
                    row.isMyTurn = false
                    board.bard(rower)(elem) = 2
        if row.sums(elem) == 1 then
          if row.isMyTurn then
            println("hey")
  }
      //TODO: add more cases for the rest of the board
      //TODO: delete the cases in placement logic
      //TODO: make sure it works

