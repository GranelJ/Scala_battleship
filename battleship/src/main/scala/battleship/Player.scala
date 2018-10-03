package battleship


case class Player (shipBoard : Board, shotBoard: Board){

  /**
    * Function to know if a Player as lost
    * @return : true if the Player doesn't have ship on his board, false otherwise
    */
  def as_Lost() : Boolean = {
      return !this.shipBoard.as_Ship()
    }
}