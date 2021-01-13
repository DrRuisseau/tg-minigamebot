package s1.telegrambots

import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.api.declarative.{Callbacks, Commands}
import info.mukel.telegrambot4s.methods
import info.mukel.telegrambot4s.methods.{ParseMode, SendMessage}
import info.mukel.telegrambot4s.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import s1.telegrambots.Games.gamesMap
import s1.telegrambots.games.Game

import scala.collection.mutable.Map
import scala.concurrent.Future
import scala.io.Source

class Chat(val chatId: Long) extends TelegramBot with Polling with Commands with Callbacks {

  def token = scala.io.Source.fromFile("bot_token.txt").mkString.trim
  type Message = info.mukel.telegrambot4s.models.Message
  type Button = InlineKeyboardButton

  var message: Message = _
  var data: String = _
  var messageId: Int = _

  var prevMsg: Option[Future[Message]] = None
  var prevUserMsg: Option[Message] = None

  var currentGame: Option[Game] = None

  var gameSelectionOngoing = false
  var gameSelectionFrom: Option[Message] = None
  var selectedGame = ""

  val gameButtons = new GameButtons(this)

  def checkIfOver(chatId: Long) = {
    currentGame.get.winner match {
      case Some(winner) => {
        if (currentGame.get.player2 == 0) request(SendMessage(chatId, users(winner) + " won the game")) else request(SendMessage(chatId, "Winner is " + users(winner)))
        currentGame = None
      }
      case None =>
    }
    currentGame match {
      case Some(game) => {
        game.loser match {
          case Some(loser) => {
            request(SendMessage(chatId, users(loser) + " lost the game"))
            currentGame = None
          }
          case None =>
        }
      }
      case None =>
    }
    if (currentGame.isDefined && currentGame.get.draw) {
      request(SendMessage(chatId, "Game ended in a draw"))
      currentGame = None
    }
  }

  var users = Map[Int, String]()
  val usersFile = "src/main/scala/s1/telegrambots/users.json"

  val file = Source.fromFile(usersFile)
  try {
    ujson.read(file.mkString)(chatId.toString)
      .obj
      .foreach(u => users(u._1.toInt) = u._2.str)
  } finally file.close()

  /** Starts the selected game.
  * @param msg The message where the command was sent from
  * @return A string describing the game state */
  def startGame(msg: Message, difficultyOrOpponent: Int): String = {
    if (!this.users.contains(msg.from.get.id)) return "You're not registered"
    val game = gamesMap(selectedGame)
    val playerId = msg.from.get.id
    if (game._1 == 1) {
      currentGame = Some(gamesMap(selectedGame)._2(playerId, difficultyOrOpponent, chatId))
    } else if (game._1 == 2) {
      currentGame = Some(gamesMap(selectedGame)._2(playerId, difficultyOrOpponent, chatId))
    }
    getGameState
  }

  /** Gets the state of the game, and returns it with info about the game.
  * @return A string containing the game board */
  def getGameState = {
    val game = currentGame.get
    val player1Id = game.player1
    val player2Id = game.player2
    if (player2Id == 0) {
      s"${game.name}: ${users(player1Id)}\n"
    } else {
      s"${game.name}:\n ${users(player1Id)} vs ${users(player2Id)}\n" + game.toString
    }
  }

  def updateMessage(newText: String, keyboard: Option[Seq[Seq[Button]]] = None) = {
    val newMarkup = keyboard match {
      case Some(kb) => Some(InlineKeyboardMarkup.apply(kb))
      case None => None
    }
    request(methods.EditMessageText(Some(chatId), Some(messageId), text = newText, parseMode = Some(ParseMode.Markdown), replyMarkup = newMarkup))
  }

}

