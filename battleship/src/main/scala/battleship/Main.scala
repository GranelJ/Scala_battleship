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
  def mainLoopHuman(gameState: GameState): Unit = {
    val currentPlayer = gameState.getActivePlayer()
    val opponentPlayer = gameState.getOpponentPlayer()
    promptBoards(currentPlayer)
    promptAskXCoordShot()
    val inputX = getUserInputInt()
    if (!inputX.isEmpty){
      promptAskYCoordShot()
      val inputY = getUserInputInt()
      if(!inputY.isEmpty){
        val nPlayerList = currentPlayer.shot(inputX.get, inputY.get, opponentPlayer)
        val nOpponent = nPlayerList.head
        val nCurrent = nPlayerList.last
        //TODO : Modify for real win message maybe name player
        if(nOpponent.as_Lost()){
          println("YOU WIN")
        }else{
          val nGameState = GameState(Set(nOpponent, nCurrent))
          mainLoopHuman(nGameState)
        }
      }else{
        println("You should enter a number between 0 and 9 !")
        mainLoopHuman(gameState)
      }
    }else{
      println("You should enter a number between 0 and 9 !")
      mainLoopHuman(gameState)
    }
  }

  def mainLoopIA(gameState: GameState, randX : Random, randY : Random, randOrien : Random) : Unit = {

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
        val gameState = GameState(Set(nplayer1, nplayer2))
        mainLoopHuman(gameState)
      }
      case Some(2) => {
        val player1 = Player(Board(), Board())
        val player2 = Player(Board(), Board(), 1)
        val nplayer1 = create_fleet(player1)
        promptBoards(nplayer1)
        val nplayer2 = create_fleet(player2)
        val gameState = GameState(Set(nplayer1, nplayer2))
      }
      case Some(3) => {
        val player1 = Player(Board(), Board())
        val player2 = Player(Board(), Board(), 2)
        val randX = Random
        val randY = Random
        val nplayer1 = create_fleet(player1)
        promptBoards(nplayer1)
        val nplayer2 = create_fleet(player2)
        val gameState = GameState(Set(nplayer1, nplayer2))

      }
      case Some(4) => {
        val player1 = Player(Board(), Board())
        val player2 = Player(Board(), Board(), 3)
        val randX = Random
        val randY = Random
        val nplayer1 = create_fleet(player1)
        promptBoards(nplayer1)
        val nplayer2 = create_fleet(player2)
        val gameState = GameState(Set(nplayer1, nplayer2))

      }
      case Some(5) => {
        val player1 = Player(Board(), Board(), 1)
        val player2 = Player(Board(), Board(), 2)
        val randX = Random
        val randY = Random
        val nplayer1 = create_fleet(player1)
        val nplayer2 = create_fleet(player2)
        val gameState = GameState(Set(nplayer1, nplayer2))

      }
      case Some(6) => {
        val player1 = Player(Board(), Board(), 2)
        val player2 = Player(Board(), Board(), 3)
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
}
