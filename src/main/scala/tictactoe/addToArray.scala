package tictactoe


/*
DONE: get it so that the computer chooses a location it thinks is best for the placement, check if that row is empty

the point of this algorithm is not to win, it's to be a nuisance, trying to constantly tie you
 */

class addToArray {
  /*
  val user = new boardAdder()
  computerLogic(board.bard).removeTwos() //automatically clones the current board to row.newBoard
  computerLogic(row.newBoard).getSum() //gets the sum of the board
  println(row.sums)
  computerLogic(row.newBoard).placementLogic()//place a 2 somewhere
  println(user.currentState(board.bard))
  println(user.currentState(row.newBoard))

  computerLogic(board.bard).removeTwos()
  computerLogic(row.newBoard).getSum()
  user.currentState(row.newBoard)
  */

  private val user = new boardAdder
  row.isMyTurn = true
  row.sums += 0
  row.sums += 10
  row.sums += 2
  user.currentState(board.bard)
  computerLogic(board.bard).findPlacement()
  user.currentState(board.bard)
  //computerLogic(board.bard).numExists(List(7,0))
  //computerLogic(board.bard).numExists(List(6,0))
}

@main def help() : Unit = {
  addToArray()
}
