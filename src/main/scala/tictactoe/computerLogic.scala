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
  private def numExists(numsToCheck : List[Int]) : ListBuffer[Boolean] = {
    val boolList = ListBuffer[Boolean]()
    for elem <- numsToCheck do
      if elem == row.sums(0) || elem == row.sums(1) || elem == row.sums(2) then
        boolList += true
      else boolList += false
    boolList
  }

  private def allTrue(boolsToCheck : ListBuffer[Boolean]) : Boolean = {
    if boolsToCheck.contains(false) then false
    else true
  }

  private def emptyRow : Int = {
    var rowToRep = -1
    for (elem <- 0 until 3) {
      if (row.sums(elem) >= 0 && row.sums(elem) <= 6) || (row.sums(elem) >= 8 && row.sums(elem) <= 10) then
        rowToRep = elem
    }
    rowToRep
  }
  private def whichRow(numToFind : Int) : Int = {
    var rowHasNum = -1
    for (elem <- 0 until 3) {
      if numToFind == row.sums(elem) then
        rowHasNum = elem
    }
    rowHasNum
  }
  override def findPlacement(): Unit ={
    //TODO: convert rowToReplace to match cases
    //cases = 11,10,7,5,2,1,_
    for elem <- row.sums do
      println(elem)
      elem match
        case 11 =>
          if row.isMyTurn then
          //check if another row is full somewhere or insert in a non-empty row
            if !allTrue(numExists(List(7,0))) then
              board.bard(emptyRow)(nextInt(3)) = 2
              row.isMyTurn = false
            if allTrue(numExists(List(7,0))) then //this is where there are two full rows above it
              if emptyRow == 2 then //if the empty row is the last one, do some logic for vertical checking
                for (colm <- 0 until 3) {
                  if board.bard(2)(colm) == 0 then //only checks verticals if the current space is 0
                    if board.bard(0)(colm) == 1 && board.bard(1)(colm) == 1 then
                      if row.isMyTurn then
                        board.bard(2)(colm) = 2 //if there is 2 ones above it, place one below them
                        row.isMyTurn = false
                    else if board.bard(0)(colm) == 2 && board.bard(1)(colm) == 2 then
                      if row.isMyTurn then
                        board.bard(2)(colm) = 2 //if we don't find two 1's but instead two 2's, place it below those
                        row.isMyTurn = false
                  if colm == 2 then //if we reach the end of the row and do not fill either one, we place at the first 0 we see
                    for (j <- 0 until 3) {
                      if row.isMyTurn then
                        if board.bard(2)(j) == 0 then
                          board.bard(2)(j) = 2
                          row.isMyTurn = false
                      }
                }
        case 10 =>
          if row.isMyTurn then
            //check if another row has two 1's in it, and place it there, otherwise place it in the row
            if allTrue(numExists(List(2))) then
              for (colm <- 0 until 3) {//place it in the row that has 2 ones
                if board.bard(whichRow(2))(colm) == 0 then
                  if row.isMyTurn then
                    board.bard(whichRow(2))(colm) = 2
                    row.isMyTurn = false
              }
            else //place it in the row with two 2's
              for (colm <- 0 until 3) {
                if board.bard(whichRow(10))(colm) == 0 then
                  if row.isMyTurn then
                    board.bard(whichRow(10))(colm) = 2
                    row.isMyTurn = false
              }
        case 7 =>
          if row.isMyTurn then
            println()
            //check if another row is full somewhere and/or insert in a non-empty row, same logic as when elem is 11
        case 5 =>
          if row.isMyTurn then
            println() //if not first row, check for vertical wins non-dynamically, if there none, place a 2 next to the other 2
        case 2 =>
          if row.isMyTurn then
            //replace with no checks
            for (colm <- 0 until 3) {
              if board.bard(whichRow(2))(colm) == 0 then
                if row.isMyTurn then
                  board.bard(whichRow(2))(colm) = 2
                  row.isMyTurn = false
            }
        case 1 =>
          if row.isMyTurn then
            if whichRow(1) == 3 then
              for (colm <- 0 until 3) {
                if board.bard(1)(colm) == 1 && board.bard(0)(colm) == 1 then
                  if row.isMyTurn then
                    board.bard(2)(colm) = 2
                    row.isMyTurn = false
                else if board.bard(1)(colm) == 2 && board.bard(0)(colm) == 2 then
                    if row.isMyTurn then
                      board.bard(2)(colm) == 2
                      row.isMyTurn = false
              }
        case 0 =>
          println() //do nothing, just need to explicitly state it so that it doesn't start filling it by accident
        case _ =>
          if row.isMyTurn then
            for (rou <- 0 until 3) {
              for (colm <- 0 until 3) {
                if row.isMyTurn then
                  if board.bard(rou)(colm) == 0 then
                    board.bard(rou)(colm) = 2
                    row.isMyTurn = false
              }
            }
  }
  private def rowToReplace(rower: Int): Unit = {
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
