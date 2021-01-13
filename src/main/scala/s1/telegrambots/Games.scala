package s1.telegrambots

import s1.telegrambots.games.{ConnectFour, Game, Minesweeper, TicTacToe}

import scala.collection.mutable.Map

object Games {

  // Set to 1 if user can select self
  val canSelectSelf = 0

  /** Difficulties for Minesweeper. */
  val difficulties = Vector("easy", "medium", "hard")

  /** The key of the map is the name used with /game, the value is a tuple, with the
  * first element being the number of players and the second being the function
  * for starting the game. */
  val gamesMap = Map[String, (Int, (Int, Int, Long) => Game)](
    "Tic-Tac-Toe" -> (2, ticTacToe),
    "Connect4"    -> (2, connectFour),
    "Minesweeper" -> (1, minesweeper),
  )

  /** Functions for starting the games. */
  def ticTacToe(player1: Int, player2: Int, chatId: Long) =
    new TicTacToe(player1, player2, chatId)


  def connectFour(player1: Int, player2: Int, chatId: Long) =
    new ConnectFour(player1, player2, chatId)


  def minesweeper(player: Int, difficulty: Int, chatId: Long) =
    new Minesweeper(player, difficulty, chatId)


}
