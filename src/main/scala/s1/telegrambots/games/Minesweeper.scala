package s1.telegrambots.games

class Minesweeper(val player1: Int, difficultyInt: Int, val chatId: Long) extends Game {

  val name = "Minesweeper"
  val player2 = 0

  val difficulty = Vector(EASY, MEDIUM, HARD, IMPOSSIBLE)(difficultyInt)
  var game = new MinesweeperGame(difficulty, player1, (0, 0))
  var firstTurn = true

  def turn(player: Int, command: String): Boolean = {
    if (player != player1) return false
    try {
      val regex = """(\d+)[^\d]+(\d+)""".r
      val inputs = regex findAllIn command
      val col = inputs.group(1).toInt
      val row = inputs.group(2).toInt
      if (firstTurn) game = new MinesweeperGame(difficulty, player1, (row, col)); firstTurn = false
      val success = this.game.reveal(col, row)
      this.game.isLost
      this.game.isWon
      success
    } catch {
      case _: Throwable => false
    }
  }

  def buttons = game.getButtons(chatId)

  def winner = if (this.game.isWon) Some(player1) else None
  def loser = if (this.game.isLost) Some(player1) else None

  override def toString: String = this.game.toString
}
