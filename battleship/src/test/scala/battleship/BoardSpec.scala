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
  "The Board check_Ship method" should "return true if the boat can be placed on the board" in {
    board.check_Ship(shipInHorizontal) shouldEqual true
    board.check_Ship(shipInVertical) shouldEqual true
    board.check_Ship(shipOutHorizontal) shouldEqual false
    board.check_Ship(shipOutVertical) shouldEqual false
    boardTest.check_Ship(shipInHorizontal) shouldEqual false
    boardTest.check_Ship(shipInVertical) shouldEqual true
   }
  
  val shipInCoordHorizontal = List.range(shipInHorizontal.x, shipInHorizontal.x + shipInHorizontal.size())
  val shipInCoordVertical = List.range(shipInVertical.y, shipInVertical.y + shipInVertical.size())
  val boardTest2 = Board(List(List(0,0,0,0,0,0,0,0,0,0),List(0,4,0,0,0,0,0,0,0,0),List(0,4,0,0,0,0,0,0,0,0),List(0,4,0,0,0,0,0,0,0,0),List(0,4,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))
  val boardTest3 = Board(List(List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0),List(0,0,0,0,0,0,0,0,0,0)))
  "The Board shipOverpass method" should "return true if a ship overpass on another" in {
    boardTest.shipOverpass(shipInHorizontal, shipInCoordHorizontal, false) shouldEqual true
    boardTest2.shipOverpass(shipInHorizontal, shipInCoordHorizontal, false) shouldEqual true
    boardTest3.shipOverpass(shipInHorizontal, shipInCoordHorizontal, false) shouldEqual false
    boardTest2.shipOverpass(shipInVertical, shipInCoordVertical, false) shouldEqual true
    boardTest3.shipOverpass(shipInVertical, shipInCoordVertical, false) shouldEqual false
   }

  //val test 1
  val ship1 = Ship(5,2,true, "Carrier")
  val shiplist1 = List.fill(10)(0)
  val shipCoord1 = List.range(ship1.x, ship1.x + ship1.size())
  //val test 2 with battleship
  val shiplist2 = List.fill(10)(0)
  val shipCoord2 = List.range(shipInHorizontal.x, shipInHorizontal.x + shipInHorizontal.size())
  "The Board create_Ship_List_Horizontal method" should "return a list with the ship number at the coord of the ship" in {
    board.create_Ship_List_Horizontal(shiplist1, shipCoord1, ship1) shouldEqual List(0, 0, 0, 0, 0, 5, 5, 5, 5, 5)
    board.create_Ship_List_Horizontal(shiplist2, shipCoord2, shipInHorizontal) shouldEqual List(0, 4, 4, 4, 4, 0, 0, 0, 0, 0)
   }

  //val test 1
  val shipVertical1 = Ship(1,1,false, "Submarine")
  val shiplistVertical1 = List.fill(shipVertical1.size())(List.fill(10)(0))
  //val test 2
  val shipVertical2 = Ship(3,5,false, "Destroyer")
  val shiplistVertical2 = List.fill(shipVertical2.size())(List.fill(10)(0))
  "The Board create_Ship_List_Vertical method" should "return a list of list with the new list of coordinates" in {
    board.create_Ship_List_Vertical(shiplistVertical1, 0, shipVertical1) shouldEqual List(List(0, 2, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 2, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 2, 0, 0, 0, 0, 0, 0, 0, 0))
    board.create_Ship_List_Vertical(shiplistVertical2, 0, shipVertical2) shouldEqual List(List(0, 0, 0, 1, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 1, 0, 0, 0, 0, 0, 0))
   }

  val shiplistVertical1done = List(List(0,2,0,0,0,0,0,0,0,0),List(0,2,0,0,0,0,0,0,0,0),List(0,2,0,0,0,0,0,0,0,0))
  val shiplistVertical2done = List(List(0,0,0,1,0,0,0,0,0,0),List(0,0,0,1,0,0,0,0,0,0))
  "The Board add_Vertical_Ship_In_Grid method" should "return a list of list with the new list of coordinates" in {
    board.add_Vertical_Ship_In_Grid(firstGrid, shiplistVertical1done, shipVertical1) shouldEqual List(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 2, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 2, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 2, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    board.add_Vertical_Ship_In_Grid(firstGrid, shiplistVertical2done, shipVertical2) shouldEqual List(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 1, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 1, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
   }

  "The Board add_Ship method" should "return a list of list with the new list of coordinates" in {
    board.add_Ship(shipVertical1) shouldEqual Board(List(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 2, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 2, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 2, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
    board.add_Ship(shipVertical2) shouldEqual Board(List(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 1, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 1, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
    board.add_Ship(ship1) shouldEqual Board(List(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 5, 5, 5, 5, 5), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
    board.add_Ship(shipInHorizontal) shouldEqual Board(List(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 4, 4, 4, 4, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
   }

  "The Board shot_As_Hit method" should "return true if there is a ship where we have shot" in {
    board.shot_As_Hit(5, 3) shouldEqual false
    boardTest.shot_As_Hit(4, 8) shouldEqual false
    boardTest.shot_As_Hit(1, 1) shouldEqual true
    boardTest2.shot_As_Hit(1, 2) shouldEqual true
   }

  "The Board update_Grid_After_Shot method" should "return the new grid with -1 where we have shot" in {
    board.update_Grid_After_Shot(5, 3) shouldEqual Board(List(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, -1, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
    boardTest.update_Grid_After_Shot(4, 8) shouldEqual Board(List(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 4, 4, 4, 4, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, -1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
    boardTest.update_Grid_After_Shot(1, 1) shouldEqual Board(List(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, -1, 4, 4, 4, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
    boardTest2.update_Grid_After_Shot(1, 2) shouldEqual Board(List(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 4, 0, 0, 0, 0, 0, 0, 0, 0), List(0, -1, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 4, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 4, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
   }

  "The Board as_Ship method" should "return false if there is no ship in the board, true otherwise" in {
    board.as_Ship() shouldEqual false
    boardTest.as_Ship() shouldEqual true
    boardTest2.as_Ship() shouldEqual true
  }
}