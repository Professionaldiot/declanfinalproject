package tictactoe


/*
TODO: get it so that the computer chooses a location it thinks is best for the placement, check if that row is empty

the point of this algorithm is not to win, it's to be a nuisance, trying to constantly tie you
 */

def makeArrayV1 = {
  val user = new boardAdder()
  user.bored(1, 0, 0)
  user.bored(2, 1, 0)
  user.bored(2, 1, 2)
  user.bored(1, 0, 1)
  user.bored(1, 2, 0)
}
def makeArrayV2 = {
  val user = new boardAdder()
  user.bored(1, 0, 0)
  user.bored(2, 1, 0)
  user.bored(2, 1, 2)
  user.bored(1, 2, 0)
  user.bored(1, 2, 1)
}
class addToArray {
  val user = new boardAdder()
  makeArrayV2
  computerLogic(board.bard).removeTwos()
  computerLogic(row.newBoard).getSum()
  println(row.sums)
  computerLogic(row.newBoard).placementLogic()
  println(user.currentState(board.bard))
  println(user.currentState(row.newBoard))


}
