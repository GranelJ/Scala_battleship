package battleship

import org.scalatest._

class BoardSpec extends FlatSpec with Matchers {
  val firstGrid = List.fill(10)(List.fill(10)(0))
  val board = Board(firstGrid)
  val boardTest = Board(List(List(0,0,0,0,0,0,0,0,0,0),List(0,4,4,4,4,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))

  "The Board object" should "be a list of list 10x10" in {
    board.grid.size shouldEqual 10
    board.grid(0).size shouldEqual 10
    board.grid(1).size shouldEqual 10
    board.grid(2).size shouldEqual 10
    board.grid(3).size shouldEqual 10
    board.grid(4).size shouldEqual 10
    board.grid(5).size shouldEqual 10
    board.grid(6).size shouldEqual 10
    board.grid(7).size shouldEqual 10
    board.grid(8).size shouldEqual 10
    board.grid(9).size shouldEqual 10
   }
  
  val shipInHorizontal = Ship(1,1,true, "Battleship")
  val shipInVertical = Ship(1,2,false, "Destroyer")
  val shipOutHorizontal = Ship(9,2,true, "Destroyer")
  val shipOutVertical = Ship(2,8,false, "Submarine")
  "The Board Check_Ship method" should "return true if the boat can be placed on the board" in {
    board.Check_Ship(shipInHorizontal) shouldEqual true
    board.Check_Ship(shipInVertical) shouldEqual true
    board.Check_Ship(shipOutHorizontal) shouldEqual false
    board.Check_Ship(shipOutVertical) shouldEqual false
    boardTest.Check_Ship(shipInHorizontal) shouldEqual false
    boardTest.Check_Ship(shipInVertical) shouldEqual true
   }
  
  val shipInCoordHorizontal = List.range(shipInHorizontal.x, shipInHorizontal.x + shipInHorizontal.size())
  val shipInCoordVertical = List.range(shipInVertical.y, shipInVertical.y + shipInVertical.size())
  val boardTest2 = Board(List(List(0,0,0,0,0,0,0,0,0,0),List(0,4,0,0,0,0,0,0,0,0),List(0,4,0,0,0,0,0,0,0,0),List(0,4,0,0,0,0,0,0,0,0),List(0,4,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))
  val boardTest3 = Board(List(List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))
  "The Board ShipOverpass method" should "return true if a ship overpass on another" in {
    boardTest.ShipOverpass(shipInHorizontal, shipInCoordHorizontal, false) shouldEqual true
    boardTest2.ShipOverpass(shipInHorizontal, shipInCoordHorizontal, false) shouldEqual true
    boardTest3.ShipOverpass(shipInHorizontal, shipInCoordHorizontal, false) shouldEqual false
    boardTest2.ShipOverpass(shipInVertical, shipInCoordVertical, false) shouldEqual true
    boardTest3.ShipOverpass(shipInVertical, shipInCoordVertical, false) shouldEqual false
   }

  //val test 1
  val ship1 = Ship(5,2,true, "Carrier")
  val shiplist1 = List.fill(10)(0)
  val shipCoord1 = List.range(ship1.x, ship1.x + ship1.size())
  //val test 2 with battleship
  val shiplist2 = List.fill(10)(0)
  val shipCoord2 = List.range(shipInHorizontal.x, shipInHorizontal.x + shipInHorizontal.size())
  "The Board Create_ship_list_horizontal method" should "return a list with the ship number at the coord of the ship" in {
    board.Create_ship_list_horizontal(shiplist1, shipCoord1, ship1) shouldEqual List(0,0,0,0,0,5,5,5,5,5)
    board.Create_ship_list_horizontal(shiplist2, shipCoord2, shipInHorizontal) shouldEqual List(0,4,4,4,4,0,0,0,0,0)
   }

  //val test 1
  val shipVertical1 = Ship(1,1,false, "Submarine")
  val shiplistVertical1 = List.fill(shipVertical1.size())(List.fill(10)(0))
  //val test 2
  val shipVertical2 = Ship(3,5,false, "Destroyer")
  val shiplistVertical2 = List.fill(shipVertical2.size())(List.fill(10)(0))
  "The Board Create_ship_list_vertical method" should "return a list of list with the new list of coordinates" in {
    board.Create_ship_list_vertical(shiplistVertical1, 0, shipVertical1) shouldEqual List(List(0,2,0,0,0,0,0,0,0,0),List(0,2,0,0,0,0,0,0,0,0),List(0,2,0,0,0,0,0,0,0,0))
    board.Create_ship_list_vertical(shiplistVertical2, 0, shipVertical2) shouldEqual List(List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0))
   }

  val shiplistVertical1done = List(List(0,2,0,0,0,0,0,0,0,0),List(0,2,0,0,0,0,0,0,0,0),List(0,2,0,0,0,0,0,0,0,0))
  val shiplistVertical2done = List(List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0))
  "The Board Add_vetical_ship_in_grid method" should "return a list of list with the new list of coordinates" in {
    board.Add_vetical_ship_in_grid(firstGrid ,shiplistVertical1done, shipVertical1) shouldEqual List(List(0,0,0,0,0,0,0,0,0,0),List(0,2,0,0,0,0,0,0,0,0),List(0,2,0,0,0,0,0,0,0,0),List(0,2,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0))
    board.Add_vetical_ship_in_grid(firstGrid ,shiplistVertical2done, shipVertical2) shouldEqual List(List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0))
   }

  "The Board Add_ship method" should "return a list of list with the new list of coordinates" in {
    board.Add_ship(shipVertical1) shouldEqual Board(List(List(0,0,0,0,0,0,0,0,0,0),List(0,2,0,0,0,0,0,0,0,0),List(0,2,0,0,0,0,0,0,0,0),List(0,2,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))
    board.Add_ship(shipVertical2) shouldEqual Board(List(List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))
    board.Add_ship(ship1) shouldEqual Board(List(List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,5,5,5,5,5),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))
    board.Add_ship(shipInHorizontal) shouldEqual Board(List(List(0,0,0,0,0,0,0,0,0,0),List(0,4,4,4,4,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))
   }

   "The Board Shot_As_Hit method" should "return true if there is a ship where we have shot" in {
    board.Shot_As_Hit(5, 3) shouldEqual false
    boardTest.Shot_As_Hit(4, 8) shouldEqual false
    boardTest.Shot_As_Hit(1,1) shouldEqual true
    boardTest2.Shot_As_Hit(1,2) shouldEqual true
   }
}
