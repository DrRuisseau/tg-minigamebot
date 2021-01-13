package s1.telegrambots.examples

import s1.telegrambots.BasicBot
import scala.collection.mutable.Buffer
import s1.telegrambots.games._
import scala.util._

/**
  * Ärsyttävä Botti joka kääntää sanat nurinpäin
  *
  * Botti saamaan reagoimaan kanavan tapahtumiin luomalla funktio/metodi joka käsittelee
  * halutuntyyppistä dataa ja joko palauttaa merkkijonon tai tekee jotain muuta saamallaan
  * datalla.
  *
  * Alla on yksinkertainen esimerkki - metodi joka ottaa merkkijonon ja kääntää sen nurin.
  * Luokassa BasicBot on joukko metodeja, joilla voit asettaa botin suorittamaan oman metodisi
  * ja tekemään tiedolla jotain. replyToString rekisteröi bottiin funktion joka saa
  * syötteekseen jokaisen kanavalle kirjoitetun merkkijonon. Se, mitä funktio
  * palauttaa lähetetään kanavalle.
  */
object ConnectFourBotTest extends App {

  val bot = new BasicBot() {

    var activeGame: ConnectFourOld = null

    val players = Buffer[String]()

    var currentPlayer: Int = 1

    def switchPlayerTurn = if(currentPlayer == 1) currentPlayer = 2 else currentPlayer = 1

    def addPlayer(name: String) = {
      if(players.length < 2)
        players += name
    }

    def createNewLobby(msg: Message) = {
      activeGame = new ConnectFourOld

      val name = getUserFirstName(msg)

      joinActiveGame(name)
      println(s"ID: $name joined the game")

      s"$name started a new Connect Four -game. Use /join to join the game."
    }

    def dropDisc(msg: Message): String = {
      val s: String = getString(msg)
      val sender = getUserFirstName(msg)
      if(activeGame != null) {
        if(sender == players(currentPlayer - 1)) {
          activeGame.dropDiscInColumn(s.toInt, currentPlayer)

          if (activeGame.winConditionSatisfied(currentPlayer)) {
            endGame(currentPlayer)
          }
          else {
            switchPlayerTurn
            activeGame.drawBoard
          }
        }
        else {
          "Wrong player!"
        }
      }
      else {
        "No game in progress, use /connectfour to start a new game, or /help to see a list of commands!"
      }

    }

    def endGame(winner: Int) = {
      val s1 = "Game ended and the winner was " + players(currentPlayer - 1) + "\n\n"
      val s = s1 + activeGame.drawBoard + "\n\n Use /connectfour to start a new game"

      // Clear game
      players.clear()
      activeGame = null
      currentPlayer = 1

      s
    }

    def joinActiveGame(name: String): Boolean = {
      if(activeGame != null) {
        if(players.length < 2) {
        players += name
        true
        }
        else {
          false
        }
      }
      else {
        false
      }
    }

    def joinGame(msg: Message): String = {
      println("Trying to join game")

      val succesful = joinActiveGame(getUserFirstName(msg))

      if(succesful)
        "Joined the game!"
      else
        "Failed to join a game."
    }

    def clearGame(msg: Message) = {
      players.clear()
      activeGame = null
      currentPlayer = 1

      "Cleared the current game..."
    }

    def viewHelp(msg: Message): String = {
      println("Calling for help")

      val hellos = Buffer("Greetings", "Hello", "Hello there", "Sup", "What up", "How's it popping", "Hi", "Hey")
      val chosenHello: String = hellos((new Random).nextInt(hellos.length))

      val s1 = "Here is a list of my commands:\n"
      val s2 = "Use /connectfour to start a new game\n"
      val s3 = "Use /join to join an existing game\n"
      val s4 = "Use /drop <number> to drop a disc in a column. For example '/drop 5'"
      val s = s1 + s2 + s3 + s4

      println("Strings complete")

      "Here is a list of my commands:\n\nUse connectfour to start a new game\n\nUse join to join an existing game\n\nUse drop <number> to drop a disc in a column."
    }
    //this.replyToMessage(dropDisc)

    this.command("connectfour", createNewLobby)
    this.command("join", joinGame)
    this.command("drop", dropDisc)
    this.command("clear", clearGame)
    this.command("help", viewHelp)

    this.run()

    println("Started")
  }


  //synchronized{wait()}
}
