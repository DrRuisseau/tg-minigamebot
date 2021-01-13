package s1.telegrambots.games

import scala.util.Random

class Mineboard(val width: Int, val height: Int, private val bombCount: Int, firstMove: (Int, Int)) {

  private var boardSolution : Array[Array[Int]] = createRandomBoard
  private var activeBoard : Array[Array[Boolean]] = Array.ofDim[Boolean](this.height, this.width)
  private var random = Random


  private def createRandomBoard = {
    var board = Array.ofDim[Int](this.height, this.width)
    var bombsPlaced = 0
    //There shouldn't be more than 50% bombs, actually it's usually around 20% even in the hardest difficulty
    if (this.bombCount >= width * height / 2)
      throw new Exception("Bomb count was way too big for the minesweeper board!")

    while (bombsPlaced < this.bombCount){

      //Make a pair with the index (x, y) or actually (row, column)
      var index = (Random.nextInt(height), Random.nextInt(width))
      //Check that there isn't a bomb ALREADY
      while (board(index._1)(index._2) == -1 || index == firstMove){
        //Need to re-randomize it.
        index = (Random.nextInt(height), Random.nextInt(width))
      }

      //We will mark this index as a bomb and use -1 for bombs.
      board(index._1)(index._2) = -1
      //Then we need to do +1 for all the neighbor indexes.
      for (i <- -1 to 1; j <- -1 to 1){
        var neighborIndex = (index._1 + i, index._2 + j)
        //This UGLY if sentence basically checks that the indexes don't go out of bounds and that we aren't changing the bombs into zeros.
        if (neighborIndex._1 >= 0 && neighborIndex._1 < height && neighborIndex._2 >= 0 && neighborIndex._2 < width && board(neighborIndex._1)(neighborIndex._2) != -1){
          board(neighborIndex._1)(neighborIndex._2) += 1
        }
      }
      bombsPlaced += 1
    }
    board
  }

  def reveal(row: Int, column: Int) : Boolean = {
    var success = false
    if (row >= 0 && row < this.height && column >= 0 && column < this.width) {
      //This will return true, if that place [row][column] hasn't been revealed before
      //So it'll tell if the revealing was successfull or not
      success = !this.activeBoard(row)(column)
      this.activeBoard(row)(column) = true
    }
    success
  }

  def getSolutionBoard : Array[Array[Int]] = this.boardSolution

  def getCurrentBoard : Array[Array[Int]] = {
    //This method will return the board with -2 everywhere that hasn't yet been revealed
    var curBoard = Array.ofDim[Int](this.height, this.width)
    for (row <- 0 until this.height; col <- 0 until this.width){
        curBoard(row)(col) = if (this.activeBoard(row)(col)) this.boardSolution(row)(col) else -2
    }
    curBoard
  }

}
