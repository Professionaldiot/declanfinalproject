package tictactoe

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.control.Breaks.*
import scala.util.Random.*

trait hasRow :
  def findPlacement() : Unit

object row:
  var isMyTurn : Boolean = false
  val possibleRows : ArrayBuffer[Any] = ArrayBuffer[Any]()
  var num : Int = 0
  var sums: ArrayBuffer[Int] = ArrayBuffer[Int]()
  var newBoard : Array[Array[Int]] = Array.ofDim[Int](3,3)
  var doesExist : Boolean = false

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
    row.sums = ArrayBuffer[Int]()
    for (i <- 0 until 3) do
      val j = computerBoard(i).sum
      row.sums += j
  }

  def placementLogic() : Unit = {
    //println(round.roundNum)
    if round.roundNum == 2 || round.roundNum == 4 then
      if row.isMyTurn then
        val rower = nextInt(3)
        val colm = nextInt(3)
        if board.bard(rower)(colm) != 1 && board.bard(rower)(colm) != 2 then
          board.bard(rower)(colm) = 2
          row.isMyTurn = false
      end if
    else
      if row.isMyTurn then rowToReplace(0)
      if row.isMyTurn then rowToReplace(1)
      if row.isMyTurn then rowToReplace(2)
  }
  def numExists(numsToCheck : List[Int]) : Unit = {
    row.doesExist = false
    for elem <- numsToCheck do
      if elem == row.sums(0) || elem == row.sums(1) || elem == row.sums(2) then
        row.doesExist = true
  }
  override def findPlacement(): Unit ={
    //TODO: convert rowToReplace to match cases
    //cases = 11,10,7,5,2,1,_
    val colm = 0
    for elem <- row.sums do
      if row.isMyTurn then
        println(elem)
        elem match
          case 11 =>
            //check if another row is full somewhere xor insert in a non-empty row
            numExists(List(7))
            if row.doesExist then
              println("hi")
          case 10 =>
            println() //check if another row has two 1's in it, and place it there, otherwise place it in the row
          case 7 =>
            //check if another row is full somewhere xor insert in a non-empty row

          case 5 =>
            println() //if not first row, check for vertical wins non-dynamically, if there none, place a 2 next to the other 2
          case 2 =>
            println() //replace with no checks
          case 1 =>
            println() //check vertical
          case _ =>
            println("not hi") //place at first 0 we see
  }
  def rowToReplace(rower: Int): Unit = {
    for (elem <- 0 until 2) do
      if row.isMyTurn then
        if row.sums(elem) == 0 then
          if elem <= 2 then
            if row.sums(elem+1) == 7 || row.sums(elem+1) == 11 then //there is at least one full row below our current row
              if elem == 1 then
                if (row.sums(elem-1) == 7 || row.sums(elem-1) == 11) && (row.sums(elem+1) == 7 || row.sums(elem+1) == 11) then
                  if row.isMyTurn then
                    board.bard(1)(1) = 2
                    row.isMyTurn = false
                    println("1")
              end if
            else if board.bard(rower)(elem) != 1 && board.bard(rower)(elem) != 2 then
              if row.isMyTurn then
                board.bard(rower)(elem) = 2
                row.isMyTurn = false
                println("2")
            end if
          else if elem == 3 then //check if both rows above it are full
              if (row.sums(elem-1) == 7 || row.sums(elem-1) == 11) && (row.sums(elem-2) == 7 || row.sums(elem-2) == 11) then
                if row.isMyTurn then
                  board.bard(rower)(1) = 2//will only happen if both rows are full
                  row.isMyTurn = false
                  println("3")
          end if
        else //if the row is 0 or not 0 check the other rows, if it is 0, use a recursive call to achieve this
          if board.bard(rower)(elem) != 1 && board.bard(rower)(elem) != 2 then // this will place a two in the row where it isn't ones
            if row.sums(elem) != 7  then
              if row.isMyTurn then
                row.isMyTurn = false
                board.bard(rower)(elem) = 2
                println("4")
            if row.sums(elem) == 7 then
              if elem <= 2 then if row.sums(elem+1) == 1 then
                if board.bard(2)(elem) == 1 && board.bard(0)(elem) == 1 then
                  if row.isMyTurn then
                    board.bard(1)(elem) = 2
                    row.isMyTurn = false
                    println("5")
          if row.sums(elem) == 10 then //this is where there are two 2's in the row
            if board.bard(rower)(elem) != 2 then
              if row.isMyTurn then
                row.isMyTurn = false
                board.bard(rower)(elem) = 2
                println("6")
          if row.sums(elem) == 5 then //this is where only one 2 is in the row
            if row.num == 1 then
              if board.bard(1)(elem) == 2 && board.bard(2)(elem) == 2 then
                if row.isMyTurn then
                  row.isMyTurn = false
                  board.bard(0)(elem) = 2
                  println("7")
            if row.num == 2 then //if there is a 2 above and below where we are horizontally, then place it there
              if board.bard(0)(elem) == 2 && board.bard(2)(elem) == 2 then
                if row.isMyTurn then
                  board.bard(rower)(elem) == 2
                  row.isMyTurn = false
                  println("8")
            if board.bard(0)(elem) == 1 && board.bard(2)(elem) == 1 then
              if row.isMyTurn then
                board.bard(rower)(elem) = 2 //if a 1 is above and below, place a 2 between them
                row.isMyTurn = false
                println("9")
            if row.num == 3 then // check for vertical win possibilities, and stop them
              if board.bard(1)(elem) == 1 && board.bard(0)(elem) == 1 then
                if board.bard(rower)(elem) == 1 && board.bard(rower)(elem) != 2 then
                  if row.isMyTurn then
                    row.isMyTurn = false
                    board.bard(rower)(elem) = 2
                    println("10")
            end if
        else //find the first 0 in the play field, and place a 2 there
          if row.isMyTurn then
            for (i <- 0 until 3) {
              for (j <- 0 until 3) {
                  if board.bard(i)(j) != 1 && board.bard(i)(j) != 2 then
                    row.isMyTurn = false
                    board.bard(i)(j) = 2
                    println("11")
              }
            }
            //TODO: BUG: when player 2 wants to move in the top left square, it doesn't and instead doesn't move at all, only placing it there after the board is reset
            //FIXED BUG: when player 1 plays on the left or right side, player 2 plays the rest of the board
  }
