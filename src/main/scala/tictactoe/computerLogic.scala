package tictactoe

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
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
    if round.roundNum == 2 || round.roundNum == 4 then
      if row.isMyTurn then
        val rower = nextInt(3)
        val colm = nextInt(3)
        if board.bard(rower)(colm) != 1 && board.bard(rower)(colm) != 2 then
          board.bard(rower)(colm) = 2
          row.isMyTurn = false
      end if
    else
      findPlacement()
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
    if (row.sums(0) == 7 || row.sums(0) == 11) && (row.sums(1) == 7 || row.sums(1) == 11) && (row.sums(2) == 7 || row.sums(2) == 11) then
      rowToRep = 3
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
    //DONE: convert rowToReplace to match cases
    //cases = 11,10,7,5,2,1,0,_
    if emptyRow != 3 then
      for elem <- row.sums do
        elem match
          case 11 =>
            if !allTrue(numExists(List(2))) then
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
                  else if emptyRow == 1 then //if the empty row is the last one, do some logic for vertical checking
                    for (colm <- 0 until 3) {
                      if board.bard(1)(colm) == 0 then //only checks verticals if the current space is 0
                        if board.bard(0)(colm) == 1 && board.bard(2)(colm) == 1 then
                          if row.isMyTurn then
                            board.bard(1)(colm) = 2 //if there is 2 ones above it, place one below them
                            row.isMyTurn = false
                          else if board.bard(0)(colm) == 2 && board.bard(2)(colm) == 2 then
                            if row.isMyTurn then
                              board.bard(1)(colm) = 2 //if we don't find two 1's but instead two 2's, place it below those
                              row.isMyTurn = false
                      if colm == 2 then //if we reach the end of the row and do not fill either one, we place at the first 0 we see
                        for (j <- 0 until 3) {
                          if row.isMyTurn then
                            if board.bard(1)(j) == 0 then
                              board.bard(1)(j) = 2
                              row.isMyTurn = false
                        }
                    }
                  else if emptyRow == 0 then //if the empty row is the last one, do some logic for vertical checking
                    for (colm <- 0 until 3) {
                      if board.bard(0)(colm) == 0 then //only checks verticals if the current space is 0
                        if board.bard(1)(colm) == 1 && board.bard(2)(colm) == 1 then
                          if row.isMyTurn then
                            board.bard(0)(colm) = 2 //if there is 2 ones above it, place one below them
                            row.isMyTurn = false
                          else if board.bard(1)(colm) == 2 && board.bard(2)(colm) == 2 then
                            if row.isMyTurn then
                              board.bard(0)(colm) = 2 //if we don't find two 1's but instead two 2's, place it below those
                              row.isMyTurn = false
                      if colm == 2 then //if we reach the end of the row and do not fill either one, we place at the first 0 we see
                        for (j <- 0 until 3) {
                          if row.isMyTurn then
                            if board.bard(0)(j) == 0 then
                              board.bard(0)(j) = 2
                              row.isMyTurn = false
                        }
                    }
                  end if
          case 10 =>
            if !allTrue(numExists(List(2))) then
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
            if !allTrue(numExists(List(2))) then
              if row.isMyTurn then
                //check if another row is full somewhere or insert in a non-empty row
                if !allTrue(numExists(List(11, 0))) then
                  board.bard(emptyRow)(nextInt(3)) = 2
                  row.isMyTurn = false
                if allTrue(numExists(List(11, 0))) then //this is where there are two full rows above it
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
                  else if emptyRow == 1 then //if the empty row is the last one, do some logic for vertical checking
                    for (colm <- 0 until 3) {
                      if board.bard(1)(colm) == 0 then //only checks verticals if the current space is 0
                        if board.bard(0)(colm) == 1 && board.bard(2)(colm) == 1 then
                          if row.isMyTurn then
                            board.bard(1)(colm) = 2 //if there is 2 ones above it, place one below them
                            row.isMyTurn = false
                          else if board.bard(0)(colm) == 2 && board.bard(2)(colm) == 2 then
                            if row.isMyTurn then
                              board.bard(1)(colm) = 2 //if we don't find two 1's but instead two 2's, place it below those
                              row.isMyTurn = false
                      if colm == 2 then //if we reach the end of the row and do not fill either one, we place at the first 0 we see
                        for (j <- 0 until 3) {
                          if row.isMyTurn then
                            if board.bard(1)(j) == 0 then
                              board.bard(1)(j) = 2
                              row.isMyTurn = false
                        }
                    }
                  else if emptyRow == 0 then //if the empty row is the last one, do some logic for vertical checking
                    for (colm <- 0 until 3) {
                      if board.bard(0)(colm) == 0 then //only checks verticals if the current space is 0
                        if board.bard(1)(colm) == 1 && board.bard(2)(colm) == 1 then
                          if row.isMyTurn then
                            board.bard(0)(colm) = 2 //if there is 2 ones above it, place one below them
                            row.isMyTurn = false
                          else if board.bard(1)(colm) == 2 && board.bard(2)(colm) == 2 then
                            if row.isMyTurn then
                              board.bard(0)(colm) = 2 //if we don't find two 1's but instead two 2's, place it below those
                              row.isMyTurn = false
                      if colm == 2 then //if we reach the end of the row and do not fill either one, we place at the first 0 we see
                        for (j <- 0 until 3) {
                          if row.isMyTurn then
                            if board.bard(0)(j) == 0 then
                              board.bard(0)(j) = 2
                              row.isMyTurn = false
                        }
                    }
                  end if
          case 5 =>
            if !allTrue(numExists(List(2))) then
              if row.isMyTurn then
              //if not first row, check for vertical wins non-dynamically, if there none, place a 2 next to the other 2
                if allTrue(numExists(List(7, 11))) then
                  for colm <- 0 until 3 do
                    if board.bard(whichRow(5))(colm) == 0 then
                      if row.isMyTurn then
                        board.bard(whichRow(5))(colm) = 2
                        row.isMyTurn = false
                    end if
                  end for
                else if !allTrue(numExists(List(2, 10))) then //as long as there isn't a better row to place at, we can place next to our two
                  for (colm <- 0 until 3) {
                    if board.bard(whichRow(5))(colm) == 2 then
                      if row.isMyTurn then
                        if colm < 2 then
                          board.bard(whichRow(5))(colm + 1) = 2
                          row.isMyTurn = false
                        else //colm is 2
                          board.bard(whichRow(5))(1) = 2
                          row.isMyTurn = false
                  }
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
            if !allTrue(numExists(List(2))) then
              //first, check for not full rows
              //then check for vertical win possibilities, as those are more likely with this number
              if emptyRow == -1 then
                //it's full somewhere, check the vertical possibilities
                if row.isMyTurn then
                  if whichRow(1) == 2 then
                    //check the last row for verticals
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
                  if whichRow(1) == 1 then
                    for (colm <- 0 until 3) {
                      if board.bard(0)(colm) == 1 && board.bard(2)(colm) == 1 then
                        if row.isMyTurn then
                          board.bard(1)(colm) = 2
                          row.isMyTurn = false
                        else if board.bard(0)(colm) == 2 && board.bard(2)(colm) == 2 then
                          if row.isMyTurn then
                            board.bard(1)(colm) == 2
                            row.isMyTurn = false
                    }
                    if whichRow(1) == 0 then
                      for (colm <- 0 until 3) {
                        if board.bard(1)(colm) == 1 && board.bard(2)(colm) == 1 then
                          if row.isMyTurn then
                            board.bard(0)(colm) = 2
                            row.isMyTurn = false
                          else if board.bard(1)(colm) == 2 && board.bard(2)(colm) == 2 then
                            if row.isMyTurn then
                              board.bard(0)(colm) == 2
                              row.isMyTurn = false
                      }
                end if
              else //both rows are not full, just place in the first 0 we see
                if row.isMyTurn then
                  for (colmn <- 0 until 3) {
                    if board.bard(whichRow(1))(colmn) == 0 then
                      if row.isMyTurn then
                        board.bard(whichRow(1))(colmn) = 2
                        row.isMyTurn = false
                  }
          case 0 =>
            println() //do nothing, just need to explicitly state it so that it doesn't start filling it by accident
          case _ =>
            if !allTrue(numExists(List(2))) then
              if row.isMyTurn then //place it at the first 0 we see
                for (rou <- 0 until 3) {
                  for (colm <- 0 until 3) {
                    if row.isMyTurn then
                      if board.bard(rou)(colm) == 0 && board.bard(rou)(colm) != 1 then
                        board.bard(rou)(colm) = 2
                        row.isMyTurn = false
                  }
                }
      end for
    else
      if emptyRow == 3 then
        println("There is a tie, no one won.")
  }
