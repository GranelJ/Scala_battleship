package battleship

import scala.util.Random
import Util._

object Main extends App{
  ChooseMod()

  //TODO : clear console en fin de tour
  /**
    * Loop to manage the game with Human
    * @param gameState : the gameState to play
    */
  def mainLoopHuman(gameState: GameState, randX : Random, randY : Random): Unit = {
    val currentPlayer = gameState.getActivePlayer()
    val opponentPlayer = gameState.getOpponentPlayer()
    val shotCoord = askCoord(currentPlayer, randX, randY)
    val nListPlayer = currentPlayer.shot(shotCoord(0), shotCoord(1), opponentPlayer)
    val nOpponent = nListPlayer.head
    val nCurrent = nListPlayer.last
    if(nOpponent.as_Lost()){
      println(s"${currentPlayer.name} WIN")
    }else{
      val nGameState = GameState(Set(nOpponent, nCurrent))
      mainLoopHuman(nGameState, randX, randY)
    }
  }

  def mainLoopIA(gameState: GameState, randX : Random, randY : Random) : Unit = {

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
        promptEnterName()
        val name1 = getUserInputString()
        val player1 = Player(Board(), Board(), name1)
        promptEnterName()
        val name2 = getUserInputString()
        val player2 = Player(Board(), Board(), name2)
        val nplayer1 = create_fleet(player1)
        promptBoards(nplayer1)
        val nplayer2 = create_fleet(player2)
        promptBoards(nplayer2)
        val randX = Random
        val randY = Random
        val gameState = GameState(Set(nplayer1, nplayer2))
        mainLoopHuman(gameState, randX, randY)
      }
      case Some(2) => {
        promptEnterName()
        val name1 = getUserInputString()
        val player1 = Player(Board(), Board(), name1)
        val player2 = Player(Board(), Board(),"AI-easy", 1)
        val nplayer1 = create_fleet(player1)
        promptBoards(nplayer1)
        val randX = Random
        val randY = Random
        val nplayer2 = create_fleet(player2)
        val gameState = GameState(Set(nplayer1, nplayer2))
        mainLoopHuman(gameState, randX, randY)
      }
      case Some(3) => {
        promptEnterName()
        val name1 = getUserInputString()
        val player1 = Player(Board(), Board(), name1)
        val player2 = Player(Board(), Board(),"AI-medium", 2)
        val randX = Random
        val randY = Random
        val nplayer1 = create_fleet(player1)
        promptBoards(nplayer1)
        val nplayer2 = create_fleet(player2)
        val gameState = GameState(Set(nplayer1, nplayer2))
        mainLoopHuman(gameState, randX, randY)
      }
      case Some(4) => {
        promptEnterName()
        val name1 = getUserInputString()
        val player1 = Player(Board(), Board(), name1)
        val player2 = Player(Board(), Board(),"AI-hard", 3)
        val randX = Random
        val randY = Random
        val nplayer1 = create_fleet(player1)
        promptBoards(nplayer1)
        val nplayer2 = create_fleet(player2)
        val gameState = GameState(Set(nplayer1, nplayer2))
        mainLoopHuman(gameState, randX, randY)
      }
      case Some(5) => {
        val player1 = Player(Board(), Board(),"AI-easy", 1)
        val player2 = Player(Board(), Board(),"AI-medium", 2)
        val randX = Random
        val randY = Random
        val nplayer1 = create_fleet(player1)
        val nplayer2 = create_fleet(player2)
        val gameState = GameState(Set(nplayer1, nplayer2))

      }
      case Some(6) => {
        val player1 = Player(Board(), Board(),"AI-medium", 2)
        val player2 = Player(Board(), Board(),"AI-hard", 3)
        val randX = Random
        val randY = Random
        val nplayer1 = create_fleet(player1)
        val nplayer2 = create_fleet(player2)
        val gameState = GameState(Set(nplayer1, nplayer2))

      }
      case _ => {
        print("You must enter a number between 1 and 6 !\n")
        ChooseMod()
      }
    }
  }

  def restart() : Unit = {

  }
}
