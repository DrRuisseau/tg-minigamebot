package s1.telegrambots.games

import info.mukel.telegrambot4s.models.InlineKeyboardButton

import scala.collection.immutable.ListMap

// Define the different difficulties
sealed trait Difficulty
case object EASY extends Difficulty
case object MEDIUM extends Difficulty
case object HARD extends Difficulty
case object IMPOSSIBLE extends Difficulty


class MinesweeperGame(val difficulty: Difficulty, val playerID: Int, firstMove: (Int, Int)) {

  private val emojis = Map(-2 -> "\uD83D\uDFE6", -1 -> "💣", 0 -> "0️⃣", 1 -> "1️⃣", 2 -> "2️⃣", 3 -> "3️⃣",
                            4 -> "4️⃣", 5 -> "5️⃣", 6 -> "6️⃣", 7 -> "7️⃣", 8 -> "8️⃣", 9 -> "9️⃣")
  private val columnEmojis = Map("☀️" -> 0, "❄️" -> 1, "💧" -> 2, "✨" -> 3, "🌎" -> 4, "⚡️" -> 5, "🌈" -> 6, "🍎" -> 7,
                                "🍉" -> 8, "🍊" -> 9, "🍌" -> 10, "🥕" -> 11, "🍐" -> 12, "🍔" -> 13, "🌽" -> 14)
  private val numberEmoji = Vector("0️⃣", "1️⃣", "2️⃣", "3️⃣", "4️⃣", "5️⃣", "6️⃣", "7️⃣", "8️⃣", "9️⃣")

  private var lost = false
  private var won = false

  //Board will be 2D array and its size will depend on the difficulty
  private val board : Mineboard = difficulty match {
    case EASY => new Mineboard(4, 6, 5, firstMove)
    case MEDIUM => new Mineboard(6, 8, 10, firstMove)
    case HARD => new Mineboard(8, 12, 20, firstMove)
    case IMPOSSIBLE => new Mineboard(10, 100, 250, firstMove)
    case _ => throw new Exception("This difficulty hasn't yet been implemented!")
  }



  def getTelegramFormattedBoard(current: Boolean): String = {
    var result = """🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥
              |💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥
              |```
              |╔╦╗┬┌┐┌┌─┐
              |║║║││││├┤
              |╩ ╩┴┘└┘└─┘
              |╔═╗┬ ┬┌─┐┌─┐┌─┐┌─┐┬─┐
              |╚═╗│││├┤ ├┤ ├─┘├┤ ├┬┘
              |╚═╝└┴┘└─┘└─┘┴  └─┘┴└─```
              |💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥
              |🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥
              |""".stripMargin
    val boardChoice = if (current) this.board.getCurrentBoard else this.board.getSolutionBoard
    //Columns have emojis for the user to pick the right column
    result += "```\nx  " + numberEmoji.take(this.board.width).mkString("") + "\n---" + "-" * (this.board.width*2.5).toInt + "\n"
    //x  ☀️❄️💧✨🌎⚡️🌈🍎🍉🍊🍌🥕🍐🍔🌽
    //0  🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦
    for (row <- 0 until this.board.height){
      result += row.toString.padTo(3, ' ') + boardChoice(row).foldLeft("")(_ + emojis(_)) + "\n"
    }
    result.dropRight(1) + "```"
  }


  def getButtons(chatId: Long) = {
    val boardChoice = if (this.isWon || this.isLost) {
      var board = this.board.getCurrentBoard
      val solBoard = this.board.getSolutionBoard
      for (i <- 0 until this.board.height; j <- 0 until this.board.width) {
        if (board(i)(j) == -2) board(i)(j) = -3
        if (solBoard(i)(j) == -1) board(i)(j) = -1
      }
      board
    } else this.board.getCurrentBoard
    val buttons: Array[Array[InlineKeyboardButton]] = Array.ofDim[InlineKeyboardButton](this.board.height, this.board.width)
    for (row <- 0 until this.board.height; col <- 0 until this.board.width) {
      val state = boardChoice(row)(col)
      buttons(row)(col) = new InlineKeyboardButton(
      if (state == -3) "\uD83D\uDD34️" else if (state == -2) "✖️" else if (state == -1) "💣" else if (state == 0) " " else state.toString,
      callbackData = Some(s"$col $row" + "/" + chatId)
      )
    }
    buttons.map(_.toSeq).toSeq
  }

  def reveal(col: Int, row: Int) : Boolean = {
    val revealed = this.board.reveal(row, col)
    if (revealed && this.board.getSolutionBoard(row)(col) == 0) {
      for (i <- row - 1 to row + 1; j <- col - 1 to col + 1) {
        this.reveal(j, i)
      }
    }
    revealed
  }

  def isWon = {
    this.won = this.board.getCurrentBoard.flatten.toVector == this.board.getSolutionBoard.flatMap(_.map(x => if (x == -1) -2 else x)).toVector
    this.won
  }

  def isLost = {
    this.lost = this.board.getCurrentBoard.exists( _.exists(_ == -1) )
    this.lost
  }

  override def toString : String = this.getTelegramFormattedBoard(!lost && !won)
}


//Just a reminder of the "OFFICIAL" Minesweeper statistics
//Easy: 8x8 board with exactly 10 mines
//Medium: 14x14 board with exactly 40 mines
//Hard: 30x16 board with exactly 99 mines
//We won't be using these since it would be way too large for a telegram bot lol

/*
REPL tests:

import s1.telegrambots.games.{EASY, HARD, MEDIUM, MinesweeperGame}
val mg = new MinesweeperGame(MEDIUM, 2, (0, 0))
*/