package s1.telegrambots.games

import info.mukel.telegrambot4s.models.InlineKeyboardButton

import scala.collection.mutable.Buffer

class TicTacToe(val player1: Int, val player2: Int, val chatId: Long) extends Game {

  val name = "Tic-Tac-Toe"

  val players = Map("❌" -> player1, "⭕" -> player2)

  var playerTurn = player1

  val board = Buffer(
    Buffer("　", "　", "　"),
    Buffer("　", "　", "　"),
    Buffer("　", "　", "　")
  )

  def printb() = board.foreach( a => println(a.mkString(" ")))

  def turn(id: Int, command: String): Boolean = {
    try {
      if (id == playerTurn){
        val cellIndex = command.toInt - 1
        val atIndex = board((cellIndex) / 3)((cellIndex) % 3)
        if (atIndex == "❌" || atIndex == "⭕") return false
        board((cellIndex) / 3)((cellIndex) % 3) = players.find( _._2 == id ).get._1
        playerTurn = if (playerTurn == player1) player2 else player1
        true
      } else false
    } catch {
      case _: Throwable => false
    }
  }

  private def areSame(a: String, b: String, c: String) = a == b && b == c && a != "　"

  def winner = {
    var winnerOption = board.find( row => areSame(row.head, row(1), row(2)) ).map( _.head )
    for (col <- 0 to 2) {
      if (areSame(board.head(col), board(1)(col), board(2)(col))) winnerOption = Some(board(col).head)
    }
    if (areSame(board.head.head, board(1)(1), board(2)(2))) winnerOption = Some(board.head.head)
    if (areSame(board.head(2), board(1)(1), board(2).head)) winnerOption = Some(board.head(2))
    winnerOption match {
      case Some(" ") => None
      case Some(winner) => Some(players(winner))
      case None => None
    }
  }
  def loser = None

  override def draw = !board.exists(_.exists(_.contains('　')))

  def buttons = {
    val buttons: Buffer[Buffer[InlineKeyboardButton]] = Buffer(Buffer(), Buffer(), Buffer())
    for (i <- 0 to 8) {
      buttons(i / 3).append(new InlineKeyboardButton(board(i / 3)(i % 3), callbackData = Some((i + 1).toString + "/" + chatId)))
    }
    buttons.map(_.toSeq).toSeq
  }

  //override def toString =  board.map( _.mkString(" ")).mkString("\n")
  override def toString: String = ""

}
