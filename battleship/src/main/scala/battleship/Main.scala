package battleship

import scala.util.Random
import Util._

object Main extends App{
  ChooseMod()

  /**
    * Function to manage the turns of the game
    * @param gameState : the gameState with the 2 players
    * @param randX : random used by the AI for X shot's coordinate
    * @param randY : random used by the AI for Y shot's coordinate
    */
  def mainLoop(gameState: GameState, randX : Random, randY : Random): Unit = {
    val currentPlayer = gameState.getActivePlayer()
    val opponentPlayer = gameState.getOpponentPlayer()
    val shotCoord = askCoord(currentPlayer, opponentPlayer, randX, randY)
    val nListPlayer = currentPlayer.shot(shotCoord(0), shotCoord(1), opponentPlayer)
    val nOpponent = nListPlayer.head
    val nCurrent = nListPlayer.last
    if(nOpponent.as_Lost()){
      println(s"${currentPlayer.name} WIN")
      val nScore = nCurrent.score + 1
      val newCurrent = nCurrent.copy(score = nScore)
      promptRestart()
      getUserInputString() match {
        case "R" => ChooseMod()
        case _ =>
      }
    }else{
      val nGameState = GameState(Set(nOpponent, nCurrent))
      mainLoop(nGameState, randX, randY)
    }
  }

  /**
    * Function to manage the turns of the game for the Proof of the AI
    * @param gameState : the gameState with the 2 players
    * @param randX : random used by the AI for X shot's coordinate
    * @param randY : random used by the AI for Y shot's coordinate
    * @param iterator : iterator to know how many game have been finished
    */
  def mainLoopProof(gameState: GameState, randX : Random, randY : Random, iterator : Int) : Unit = {
      val currentPlayer = gameState.getActivePlayer()
      val opponentPlayer = gameState.getOpponentPlayer()
      val shotCoord = askCoord(currentPlayer, opponentPlayer, randX, randY)
      val nListPlayer = currentPlayer.shot(shotCoord(0), shotCoord(1), opponentPlayer)
      val nOpponent = nListPlayer.head
      val nCurrent = nListPlayer.last
      if(nOpponent.as_Lost()){
        val nScore = nCurrent.score + 1
        val nIterator = iterator + 1
        //reset the boards
        val newOpponent = nOpponent.copy(shipBoard = Board(), shotBoard = Board())
        val newCurrent = nCurrent.copy(shipBoard = Board(), shotBoard = Board(),score = nScore)
        ProofFleet(newOpponent, newCurrent, randX, randY, nIterator)
      }else{
        val nGameState = GameState(Set(nOpponent, nCurrent))
        mainLoopProof(nGameState, randX, randY, iterator)
      }
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
        mainLoop(gameState, randX, randY)
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
        mainLoop(gameState, randX, randY)
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
        mainLoop(gameState, randX, randY)
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
        mainLoop(gameState, randX, randY)
      }
      case Some(5) => {
        val player1 = Player(Board(), Board(),"AI-easy", 1)
        val player2 = Player(Board(), Board(),"AI-medium", 2)
        val player3 = Player(Board(), Board(),"AI-hard", 3)
        val randX = Random
        val randY = Random
        val gameState = GameState(Set(player1, player2))
        ProofFleet(player1, player2, randX, randY, 0)
        ProofFleet(player2, player3, randX, randY, 0)
        ProofFleet(player1, player3, randX, randY, 0)
        writeCSV()
      }
      case _ => {
        print("You must enter a number between 1 and 6 !\n")
        ChooseMod()
      }
    }
  }

  /**
    * Function to manage the restart for proof loop
    * @param player1 : first player of the game
    * @param player2 : second player of the game
    * @param randX : randX used by the AI to generate a value for the X coordinate of the shot
    * @param randY : randY used by the AI to generate a value for the Y coordinate of the shot
    * @param iterator : iterator to know how much game are done
    */
  def ProofFleet(player1 : Player, player2 : Player, randX : Random, randY : Random, iterator : Int) : Unit = {
    if(iterator == 100){
      println(s"${player1.name} number of win : ${player1.score}")
      println(s"${player2.name} number of win : ${player2.score}")
      prepCSV(player1,player2)
    }else{
      val nPlayer1 = create_fleet(player1)
      val nPlayer2 = create_fleet(player2)
      val gameState = GameState(Set(nPlayer1, nPlayer2))
      mainLoopProof(gameState, randX, randY, iterator)
    }
  }
}
