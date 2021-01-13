package s1.telegrambots.games

import info.mukel.telegrambot4s.models.InlineKeyboardButton

trait Game {
  val name: String
  val chatId: Long
  val player1: Int
  val player2: Int
  def turn(id: Int, command: String): Boolean
  def winner: Option[Int]
  def loser: Option[Int]
  def draw = false
  def buttons: Seq[Seq[InlineKeyboardButton]]
}
