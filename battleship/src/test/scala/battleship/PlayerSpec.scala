package battleship

import org.scalatest._

class PlayerSpec extends FlatSpec with Matchers {
  val firstGrid = List.fill(10)(List.fill(10)(0))
  val shipGrid = Board(firstGrid)
  val shotGrid = Board(firstGrid)
  val player = Player(shipGrid, shotGrid)
  "The Player object" should "have 2 board filled with 0" in {
    player.shipBoard shouldEqual Board(firstGrid)
    player.shotBoard shouldEqual Board(firstGrid)
    }
}
