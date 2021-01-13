package s1.telegrambots.games

import scala.collection.mutable.Buffer

class ConnectFourOld {

  // ⚫️🔵🔴, Player 1 is blue, player 2 is red

  val columns = Buffer[Buffer[Slot]]()

  val blueDiscs = Buffer[Coords]()
  val redDiscs = Buffer[Coords]()

  // Perform start operations to ensure right amount of rows and slots.
  for(i <- 0 until 7) {
    val thisBuffer = Buffer[Slot]()
    for(j <- 0 until 6) {
      thisBuffer += new Slot
    }
    columns += thisBuffer
  }

  // Check that a player cannot overflow the board!!!
  def dropDiscInColumn (column: Int, player: Int): Unit = {
    val thisColumn = columns(column - 1)

    for(i <- 0 to thisColumn.length) {
      if(i == thisColumn.length || !thisColumn(i).isEmpty) {
        // Drop a disc in the slot of previous index
        thisColumn(i - 1).disc = if(player == 1) "🔵" else "🔴"

        if(player == 1)
          blueDiscs += new Coords(column - 1, i - 1)
        else
          redDiscs += new Coords(column - 1, i - 1)

        return
      }
    }
  }

  def drawBoard: String = {
    var s: String = "1️⃣2️⃣3️⃣4️⃣5️⃣6️⃣7️⃣\n"
    for(i <- 0 until 6) {
      for(j <- 0 until 7) {
        s += columns(j)(i).disc
      }
      s += "\n"
    }
    s
  }

  def winConditionSatisfied(player: Int): Boolean = {
    var disc: String = if(player == 1) "🔵" else "🔴"

    verticalWinCondition(disc) || horizontalWinCondition(disc) || diagonalWinCondition(disc)
  }

  def verticalWinCondition(disc: String): Boolean = {
    var satisfied: Boolean = false

    for(i <-  5 to 3 by -1) {
      for(j <- 0 to 6) {
        if(columns(j)(i).disc == disc) {
          var chainBroken: Boolean = false
          var t: Int = 1
          while(!chainBroken && !satisfied) {
              if (columns(j)(i - t).disc == disc)
                t += 1
              else
                chainBroken = true

              if(t > 3)
                satisfied = true
          }
        }
      }
    }

    satisfied

  }

  def horizontalWinCondition(disc: String): Boolean = {
    var satisfied: Boolean = false

    for(i <-  0 to 3) {
      for(j <- 0 to 5) {
        if(columns(i)(j).disc == disc) {
          var chainBroken: Boolean = false
          var t: Int = 1
          while(!chainBroken && !satisfied) {
              if (columns(i + t)(j).disc == disc)
                t += 1
              else
                chainBroken = true

              if(t > 3)
                satisfied = true
          }
        }
      }
    }

    satisfied

  }

  def diagonalWinCondition(disc: String): Boolean = {
    var satisfied: Boolean = false

      for(i <- 0 to 2) {
        for(j <- 0 to 3) {
          if(columns(j)(i).disc == disc) {
            var chainBroken: Boolean = false
            var t: Int = 1
            while(!chainBroken && !satisfied) {
              if (columns(j + t)(i + t).disc == disc)
                t += 1
              else
                chainBroken = true

              if(t > 3)
                satisfied = true
            }
          }
        }
      }
      for(i <- 5 to 3 by -1) {
        for(j <- 0 to 3) {
          if(columns(j)(i).disc == disc) {
            var chainBroken: Boolean = false
            var t: Int = 1
            while(!chainBroken && !satisfied) {
              println("Looping..." + s" (from row $i, with additional index $t)")

              if (columns(j + t)(i - t).disc == disc) {
                t += 1
                println("Found a disc" + s" (from row $i, with additional index $t)")
              } else {
              chainBroken = true
              println("Chain broken" + s" (from row $i, with additional index $t)")
            }

              if(t > 3)
                satisfied = true
            }
          }
        }
      }

    satisfied
  }
}

/*
import s1.telegrambots.games._
val game = new ConnectFour
game.dropDiscInColumn(1, 1)
game.dropDiscInColumn(2, 1)
game.dropDiscInColumn(2, 1)
game.dropDiscInColumn(3, 1)
game.dropDiscInColumn(3, 1)
game.dropDiscInColumn(3, 1)
game.dropDiscInColumn(4, 1)
game.dropDiscInColumn(4, 1)
game.dropDiscInColumn(4, 1)
game.dropDiscInColumn(4, 1)
game.diagonalWinCondition("🔵")
 */
