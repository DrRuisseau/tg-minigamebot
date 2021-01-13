package s1.telegrambots

import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.api.declarative.{Callbacks, Commands}
import info.mukel.telegrambot4s.methods.AnswerCallbackQuery
import info.mukel.telegrambot4s.models.{CallbackQuery, InlineKeyboardButton}
import s1.telegrambots.Games.{canSelectSelf, difficulties, gamesMap}

import scala.util.{Failure, Success}

class GameButtons(chat: Chat) extends TelegramBot with Polling with Commands with Callbacks {

  def token = scala.io.Source.fromFile("bot_token.txt").mkString.trim
  type Button = InlineKeyboardButton

  val difficultyKb =
    difficulties
      .toSeq
      .map( difficulty =>
        Seq(new Button(difficulty, callbackData = Some(difficulty + "/" + chat.chatId)))
      )

  def usersKb(id: Int) =
    chat.users
      .filter( _._1 != id + canSelectSelf )
      .toSeq
      .map( user =>
        Seq(new Button(user._2, callbackData = Some(user._2 + "/" + chat.chatId)))
      )

  def gamesKb =
    gamesMap
      .filter( _._2._1 <= chat.users.size || Games.canSelectSelf == 1 )
      .toSeq
      .map( game =>
        Seq(new Button(game._1, callbackData = Some(game._1 + "/" + chat.chatId)))
      )


  def buttonCallback(cbString: String, cbq: CallbackQuery) = {

    var message: String = ""
    var buttons: Option[Seq[Seq[Button]]] = None
    var after: Option[Long => Unit] = None
    def setAfter(parameter: Long, func: Long => Unit) = after = Some(parameter => func(parameter))

    val gameSelectionFrom = chat.gameSelectionFrom
    val gameSelectionOngoing = chat.gameSelectionOngoing
    val selectedGame = chat.selectedGame

    // Action for game turns
    if (chat.currentGame.isDefined && (cbq.from.id == chat.currentGame.get.player1 || cbq.from.id == chat.currentGame.get.player2)) {
      val success = chat.currentGame.get.turn(cbq.from.id, cbString)
      if (success) {
        message = chat.getGameState
        buttons = Some(chat.currentGame.get.buttons)
        setAfter(gameSelectionFrom.get.chat.id, chat.checkIfOver)
      } else if (chat.currentGame.get.player2 != 0) {
        request(AnswerCallbackQuery(cbq.id, text = Some("It's not your turn")))
      } else {
        request(AnswerCallbackQuery(cbq.id))
      }

    // Answer callbackquery from user who isn't selecting a game
    } else if (gameSelectionOngoing && cbq.from.id != gameSelectionFrom.get.from.get.id) {
      request(AnswerCallbackQuery(cbq.id))

    // Selecting opponent or difficulty
    } else if (gamesMap.contains(cbString)) {
      chat.selectedGame = cbString
      val players = gamesMap(chat.selectedGame)._1
      // 2 player game
      if (players == 2) {
        val kb = usersKb(gameSelectionFrom.get.from.get.id)
        if (kb.isEmpty) {
          message = "No other users"
          chat.gameSelectionOngoing = false
          chat.prevUserMsg = None
        } else {
          message = "Select opponent"
          buttons = Some(kb)
        }
      // Singleplayer game
      } else if (players == 1) {
        message = "Select difficulty"
        buttons = Some(difficultyKb)
      }

    // Start singleplayer game
    } else if (difficulties.contains(cbString)) {
      message = chat.startGame(gameSelectionFrom.get, difficulties.indexOf(cbString))
      buttons = Some(chat.currentGame.get.buttons)
      chat.gameSelectionOngoing = false
      chat.prevUserMsg = None

    // Start 2 player game
    } else if (chat.users.exists(_._2 == cbString)) {
      message = chat.startGame(gameSelectionFrom.get, chat.users.find(_._2 == cbString).get._1)
      buttons = Some(chat.currentGame.get.buttons)
      chat.gameSelectionOngoing = false
      chat.prevUserMsg = None

    // Answer wrong inputs
    } else {
      request(AnswerCallbackQuery(cbq.id))
    }


    // Update message and keyboard
    if (message != "") {
      chat.updateMessage(message, buttons).onComplete {
        case Success(_) => {
          request(AnswerCallbackQuery(cbq.id))
          if (after.isDefined) {
            after.get.apply(gameSelectionFrom.get.chat.id)
          }
        }
        case Failure(e) => println(e)
      }
    }
  }
}
