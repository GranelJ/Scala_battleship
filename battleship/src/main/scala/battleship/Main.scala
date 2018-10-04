package battleship

import scala.util.Random
import Util._

object Main extends App{
  val players = ChooseMod()

  /**
    * Loop to manage the game
    * @param gameState : the gameState to play
    */
  def mainLoop(gameState: GameState): Unit = {
    val player1 = gameState.getActivePlayer()
  }

  /**
    * Function that manage when the user choose his mod
    * @return : gameState used for the main loop
    */
  def ChooseMod() : Unit = {
    promptModChoices()
    val input = getUserInputInt()
    input match{
      case Some(1) => {
        val player1 = Player(Board(), Board())
        val player2 = Player(Board(), Board())
        val nplayer1 = create_fleet(player1)
        promptBoards(nplayer1)
        val nplayer2 = create_fleet(player2)
        promptBoards(nplayer2)
        val gameState = GameState(Set(player1, player2))
      }
      case Some(2) => {
        val player1 = Player(Board(), Board())
        val player2 = Player(Board(), Board(), 1)
        val nplayer1 = create_fleet(player1)
        promptBoards(nplayer1)
        val nplayer2 = create_fleet(player2)
      }
      case Some(3) => {
        val player1 = Player(Board(), Board())
        val player2 = Player(Board(), Board(), 2)
        val randX = Random
        val randY = Random
        val nplayer1 = create_fleet(player1)
        promptBoards(nplayer1)
        val nplayer2 = create_fleet(player2)
        val gameState = GameState(Set(player1, player2))

      }
      case Some(4) => {
        val player1 = Player(Board(), Board())
        val player2 = Player(Board(), Board(), 3)
        val randX = Random
        val randY = Random
        val nplayer1 = create_fleet(player1)
        promptBoards(nplayer1)
        val nplayer2 = create_fleet(player2)
        val gameState = GameState(Set(player1, player2))

      }
      case Some(5) => {
        val player1 = Player(Board(), Board(), 1)
        val player2 = Player(Board(), Board(), 2)
        val randX = Random
        val randY = Random
        val nplayer1 = create_fleet(player1)
        val nplayer2 = create_fleet(player2)
        val gameState = GameState(Set(player1, player2))

      }
      case Some(6) => {
        val player1 = Player(Board(), Board(), 2)
        val player2 = Player(Board(), Board(), 3)
        val randX = Random
        val randY = Random
        val nplayer1 = create_fleet(player1)
        val nplayer2 = create_fleet(player2)
        val gameState = GameState(Set(player1, player2))

      }
      case _ => {
        print("You must enter a number between 1 and 6 !\n")
        ChooseMod()
      }
    }
  }
}
