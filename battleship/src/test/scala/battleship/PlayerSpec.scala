package battleship

import org.scalatest._

class PlayerSpec extends FlatSpec with Matchers {
  val firstGrid = List.fill(10)(List.fill(10)(0))
  val shipGrid = Board(firstGrid)
  val shotGrid = Board(firstGrid)
  val shipGrid2 = Board(List(List(0,0,0,0,-1,0,0,0,0,0),List(0,4,4,4,-1,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,0,0,-1,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))
  val shotGrid2 = Board(List(List(0,0,0,0,0,-1,0,0,0,0),List(0,0,0,0,-1,0,0,0,0,0),List(0,0,0,0,-1,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,-1,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,-1,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))
  val player = Player(shipGrid, shotGrid)
  val player2 = Player(shipGrid2, shotGrid2)

  "The Player method as_Lost" should "return true if the Player as lost" in {
    player.as_Lost() shouldEqual true
    player2.as_Lost() shouldEqual false
    }
}
