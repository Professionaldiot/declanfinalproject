package tictactoe

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

trait hasRow :
  def rowToReplace(row : Int, urgent : Boolean) : Unit
  def thisRowNOW(row : Int, urgent : Boolean) = rowToReplace(row, true)

object row:
  val possibleRows : ArrayBuffer[Any] = ArrayBuffer[Any]()
  var num : Int = 0
  val sums: ArrayBuffer[Any] = ArrayBuffer[Any]()
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
    for elem <- row.sums do
      elem match
        case 10 => rowToReplace(row.num, false) //only two fives could satisfy this condition, if it's comps turn, then finish the row
        case 7 => println("7") //row is full, skip, it's 5, 1, 1, or 2, 1, 1
        case 6 => println("6") //only option is a 5 and one 1 in the row, not too urgent
        case 5 => println("5") // only option is one 5 in the row, aka player 2 has only placed one 2 in this row
        case 2 => thisRowNOW(row.num, true) //always a pair of ones, stop it in it's tracks, even if it means passing an opportunity to win
        case 1 => println("1") //player 1 has only played once in this row, not too urgent
        case 0 => println("0")
        case _ => println("greater than 8")
      row.num += 1
  }
  override def rowToReplace(row: Int, urgent : Boolean): Unit = {
    for (elem <- 0 until 3) do
      println(board.bard(row)(elem))
  }
