package battleship

import org.scalatest._

class GameStateSpec extends FlatSpec with Matchers {
  val boardTest : Board = Board(List(List(0,0,0,0,0,0,0,0,0,0),List(0,4,4,4,4,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))
  val player : Player = Player(boardTest, boardTest, "test1")
  val player2 : Player = Player(boardTest, boardTest, "test2")
  val gameState = GameState(Set(player, player2))

  "The GameState method getActivePlayer" should "return the player currently playing" in {
    gameState.getActivePlayer() shouldEqual player
  }

  "The GameState method getOpponentPlyer" should "return the player currently not playing" in {
    gameState.getOpponentPlayer() shouldEqual player2
  }
}