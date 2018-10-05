package battleship

import org.scalatest._

class PlayerSpec extends FlatSpec with Matchers {
  val shipGrid : Board = Board()
  val shotGrid : Board = Board()
  val shipGrid2 : Board = Board(List(List(0,0,0,0,-1,0,0,0,0,0),List(0,4,4,4,-1,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,0,0,-1,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))
  val shotGrid2 : Board = Board(List(List(0,0,0,0,0,-1,0,0,0,0),List(0,0,0,0,-1,0,0,0,0,0),List(0,0,0,0,-1,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,-1,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,-1,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))
  val player : Player = Player(shipGrid, shotGrid, "test1", 1)
  val player2 : Player = Player(shipGrid2, shotGrid2, "test2")

  "The Player method as_Lost" should "return true if the Player as lost" in {
    player.as_Lost() shouldEqual true
    player2.as_Lost() shouldEqual false
    }

  "The Player method create_Ship" should "return a new player with his shipboard updated if the ship can be place" in {
    player.create_Ship(Some(1),Some(2),"H","Destroyer").get shouldEqual player.copy(shipBoard = Board(List(List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,1,1,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0))), shipList = player.shipList :+ Ship(1,2,true,"Destroyer"))
    player2.create_Ship(Some(1),Some(1),"V","Submarine") shouldEqual None
    player.create_Ship(None, Some(1),"V","Submarine") shouldEqual None
  }

  "The Player method shot" should "return the List of Player with Players updated" in {
    player.shot(4, 4, player2) shouldEqual List(player2.copy(shipBoard = Board(List(List(0,0,0,0,-1,0,0,0,0,0),List(0,4,4,4,-1,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,1,-1,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,0,0,-1,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))), player.copy(shotBoard = Board(List(List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,-1,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0)))))
    player2.shot(1, 1,player) shouldEqual List(player.copy(shipBoard = Board(List(List(0,0,0,0,0,0,0,0,0,0), List(0,-1,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0)))), player2.copy(shotBoard = Board(List(List(0,0,0,0,0,-1,0,0,0,0),List(0,-1,0,0,-1,0,0,0,0,0),List(0,0,0,0,-1,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,-1,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,-1,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))))
    player.shot(7, 1, player2) shouldEqual List(player2.copy(shipBoard = Board(List(List(0,0,0,0,-1,0,0,0,0,0),List(0,4,4,4,-1,0,0,-1,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,0,0,-1,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))), player.copy(shotBoard = Board(List(List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,-1,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0), List(0,0,0,0,0,0,0,0,0,0)))))
  }
}
