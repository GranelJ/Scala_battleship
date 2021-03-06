package battleship

import org.scalatest._

class ShipSpec extends FlatSpec with Matchers {
  val ship1 : Ship = Ship(1,2,true,"Carrier")
  val ship2 : Ship = Ship(1,2,false, "Destroyer")
  val ship3 : Ship = Ship(5,2,true, "Submarine")

  "The Ship method number" should "return the number of the ship" in {
    ship1.number() shouldEqual 5
    ship2.number() shouldEqual 1
    ship3.number() shouldEqual 2
  }

  "The Ship method size" should "return the length of the ship" in {
    ship1.size() shouldEqual 5
    ship2.size() shouldEqual 2
    ship3.size() shouldEqual 3
  }

  val boardTestShip1 : Board = Board(List(List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,5,5,5,5,5,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))
  val boardTest2Ship1 : Board = Board(List(List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,5,5,5,-1,-1,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,-1,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,-1,0,0,0,0,0),List(0,0,0,0,0,0,1,1,0,0),List(0,0,0,0,0,0,0,0,0,0)))
  val boardTest3Ship1 : Board = Board(List(List(0,0,0,0,0,0,0,0,0,2),List(0,0,-1,-1,-1,-1,-1,0,0,2),List(0,-1,-1,-1,-1,-1,0,0,0,2),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,-1,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,-1,0,0,0,0,0),List(0,0,0,0,0,0,1,1,0,0),List(0,0,0,0,0,0,0,0,0,0)))
  "The Ship method is_Sunk" should "return true if the ship is sunk" in {
    ship1.is_Sunk(boardTestShip1) shouldEqual false
    ship1.is_Sunk(boardTest2Ship1) shouldEqual false
    ship1.is_Sunk(boardTest3Ship1) shouldEqual true
  }
}
