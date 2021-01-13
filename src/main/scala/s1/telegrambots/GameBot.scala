package s1.telegrambots

import java.io.FileWriter

import info.mukel.telegrambot4s.methods.SendMessage
import info.mukel.telegrambot4s.models.{ChatId, InlineKeyboardMarkup}
import s1.telegrambots.Games.gamesMap

import scala.collection.mutable.Map
import scala.io.Source

object GameBot extends App {

  val bot = new BasicBot() {

    val usersFile = "src/main/scala/s1/telegrambots/users.json"

    val chats = Map[Long, Chat]()

    val file = Source.fromFile(usersFile)
    try {
      ujson.read(file.mkString)
        .obj
        .foreach(c => {
          val chatId = c._1.toLong
          chats(chatId) = new Chat(chatId)
        })
    } finally file.close()

    val useStart = "Use /start first"

    this.command("start", (msg: Message) => {
      val id = msg.chat.id
      if (chats.contains(id)) {
        "Already started"
      } else {
        val file = Source.fromFile(usersFile)
        val json = try {
          ujson.read(file.mkString)
        } finally file.close()
        val fw = new FileWriter(usersFile, false)
        try {
          json(id.toString) = ujson.Obj()
          fw.write(ujson.write(json, indent = 2))
        } finally fw.close()
        chats(id) = new Chat(id)
        "Started. Use /help to see commands"
      }
    })


    onCallbackQuery { implicit cbq =>
      val data  = cbq.data.get

      val cbStringArgs = data.split("/")

      val chat = chats(cbStringArgs(1).toLong)

      chat.data      = data
      chat.message   = cbq.message.get
      chat.messageId = chat.message.messageId

      chat.gameButtons.buttonCallback(cbStringArgs(0), cbq)
    }


    /** When the command is called, tries to start a game and stores
      * the sender's Message object in a variable. */
    onCommand("game") { implicit msg =>
      val chatId = msg.chat.id
      if (!chats.contains(chatId)) request(SendMessage(chatId, useStart))
      else {
        val chat = chats(chatId)
        val kb = InlineKeyboardMarkup.apply(chat.gameButtons.gamesKb)
        if (chat.currentGame.isDefined) {
          request(SendMessage(chatId, "There is already an ongoing game"))
        } else if (!chat.users.contains(msg.from.get.id)) {
          request(SendMessage(chatId, "You're not registered"))
        } else if (chat.gameSelectionOngoing) {
          request(SendMessage(chatId, "Someone else is selecting a game"))
        } else {
          request(SendMessage(chatId, "Select a game", replyMarkup = Some(kb)))
          chat.gameSelectionOngoing = true
          chat.gameSelectionFrom = Some(msg)
        }
      }
    }


    onCommand("fix") { implicit msg =>
      val chatId = msg.chat.id
      if (chats.contains(chatId)) chats(chatId).gameSelectionOngoing = false
    }


    /** Tries to stop the ongoing game.
      * @param msg The message where the command was sent from
      * @return A string saying if the game was stopped */
    def stopGame(msg: Message): String = {
      val chatId = msg.chat.id
      if (!chats.contains(chatId)) return useStart
      val chat = chats(chatId)
      if (chat.currentGame.isEmpty) {
        "There's no game to stop"
      } else if ( msg.from.get.id == chat.currentGame.get.player1 || msg.from.get.id == chat.currentGame.get.player2 ) {
        chat.currentGame = None
        "Stopped the game"
      } else {
        "You can't stop the game if you aren't playing"
      }
    }

    /** Tries to add the user to a Map.
      * @param msg The message where the command was sent from
      * @return A string telling if adding the user was successful */
    def register(msg: Message): String = {
      val chatId = msg.chat.id
      if (!chats.contains(chatId)) return useStart
      val users = chats(chatId).users

      val username = msg.from.get.username
      val id = msg.from.get.id
      if (users.contains(id)) return "You're already registered"
      username match {
        case Some(name) =>
          val atname = "@" + name
          users += id -> atname
          val file = Source.fromFile(usersFile)
          val json = try {
            ujson.read(file.mkString)
          } finally file.close()
          val fw = new FileWriter(usersFile, false)
          try {
            json(chatId.toString)(id.toString) = atname
            fw.write(ujson.write(json, indent = 2))
          } finally fw.close()
          "Registered as " + atname
        case None =>
          "You need a Telegram username to do that"
      }
    }


    /** Gets the registered users from the users Map
      * @param msg The message where the command was sent from
      * @return A string containing all the registered users if there are any */
    def getUsers(msg: Message): String = {
      val chatId = msg.chat.id
      if (!chats.contains(chatId)) return useStart
      val users = chats(chatId).users
      if (users.isEmpty) "No registered users" else "Registered users:\n " + users.values.mkString("\n ")
    }


    /** Command for listing the commands
      * @param msg The message where the command was sent from
      * @return A string containing all the commands and their uses */
    def help(msg: Message): String = {
      this.specialCommands.map( c => c._1 + " - " + c._2 ).mkString("\n") + "\n" + this.commands.map( c => c._1 + " - " + c._3 ).mkString("\n")
    }


    /** Command for listing the games
      * @param msg The message where the command was sent from
      * @return A string containing the names of the games */
    def listGames(msg: Message) = {
      "Available games:\n" + gamesMap.map( game => s" ${game._1} - ${game._2._1}p").mkString("\n")
    }


    val specialCommands = Vector(
      ("/game", "Starts a game"),
    )


    val commands = Vector[(String, Message => String, String)](
      ("/games", listGames, "Lists the games"),
      ("/stop", stopGame, "Stops the current game"),
      ("/register", register, "Registers you as an user"),
      ("/users", getUsers, "Lists registered users"),
      ("/help", help, "Shows this help message"),
    )


    /** Initiates the commands from the commands Vector */
    this.commands.foreach( c => this.command(c._1, c._2) )


    this.run()


    println("Started")
  }


}
