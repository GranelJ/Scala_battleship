package battleship

import Util._

//iA = 0 means player is human
case class Player(shipBoard : Board, shotBoard: Board, iA : Int = 0, shipList : List[Ship] = List()){

  /**
    * Function to know if a Player as lost
    * @return : true if the Player doesn't have ship on his board, false otherwise
    */
  def as_Lost() : Boolean = {
    return !this.shipBoard.as_Ship()
  }

  /**
    * Function to create a Ship
    * @param name : the name of the ship to create
    * @return : the Ship if every input id ok, None otherwise
    */
  def create_Ship(x : Option[Int] , y: Option[Int], orientation: String, name : String): Option[Player] = {
    if(!x.isEmpty){
      if(!y.isEmpty){
        var inputOrientation = true
        orientation match {
          case "H" => inputOrientation = true
          case "V" => inputOrientation = false
          case _ => None
        }
        val ship = Ship(x.get, y.get, inputOrientation, name)
        if (this.shipBoard.check_Ship(ship)){
          val nBoard = this.shipBoard.add_Ship(ship)
          val nPlayer = Some(this.copy(shipBoard = nBoard, shipList = this.shipList :+ ship))
          return nPlayer
        }else{
          return None
        }
      }
      return None
    }
    return None
  }

  def shot(x : Int,  y: Int, opponent : Player): List[Player] ={
    if(this.iA == 0){
      //Update shot grid of currently playing Player
      val nShotBoard = this.shotBoard.update_Grid_After_Shot(x, y)
      //if hit
      if(opponent.shipBoard.shot_As_Hit(x,y)){
        promptShotHit()
        //Update opponent shipboard so he can see where current player as shot
        val nOpponentShipBoard = opponent.shipBoard.update_Grid_After_Shot(x,y)
        //Update the opponent with his new shipboard
        val nOpponent = opponent.copy(shipBoard = nOpponentShipBoard)
        //Look if a ship is sunk and return his index, -1 if no ship are sunk
        val index = nOpponent.shipList.indexWhere(ship => ship.is_Sunk(nOpponent.shipBoard))
        //if no ship is sunk
        if(index == -1){
          //Update de players
          val nCurrent = this.copy(shotBoard = nShotBoard)
          return List(nOpponent, nCurrent)
        }else{
          promptShipSunk(nOpponent.shipList(index))
          //Get the sunk ship
          val sunkShip = nOpponent.shipList(index)
          //Update opponent shipList
          val nOpponentShipList = nOpponent.shipList.filter(ship => ship != sunkShip)
          print(nOpponentShipList)
          //Update de players
          val newOpponent = nOpponent.copy(shipList = nOpponentShipList)
          val nCurrent = this.copy(shotBoard = nShotBoard)
          return List(newOpponent, nCurrent)
        }
      }else{
        promptShotMiss()
        //Update opponent shipboard so he can see where current player as shot
        val nOpponentShipBoard = opponent.shipBoard.update_Grid_After_Shot(x,y)
        //Update de players
        val nOpponent = opponent.copy(shipBoard = nOpponentShipBoard)
        val nCurrent = this.copy(shotBoard = nShotBoard)
        return List(nOpponent, nCurrent)
      }/**TODO : AI
    }else if (this.iA == 1){

    }else if (this.iA == 2){
    **/
    }else{
      print("PAS D'IA")
      return List(this, opponent)
    }
  }
}
