package battleship

import Util._

//iA = 0 means player is human
case class Player(shipBoard : Board, shotBoard: Board, iA : Int = 0){

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
          val nPlayer = Some(this.copy(shipBoard = nBoard))
          return nPlayer
        }else{
          return None
        }
      }
      return None
    }
    return None
  }

  def shot(x : Int,  y: Int): Unit ={

  }
}
