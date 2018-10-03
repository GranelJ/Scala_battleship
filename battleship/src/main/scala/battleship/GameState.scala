package battleship

case class GameState(players : Set[Player]) {

  /**
    * Function to get the Player who is playing
    * @return : the Player currently playing
    */
    def getActivePlayer() : Player = {
      return this.players.head
    }

  /**
    * Function to get the Player who isn't playing
    * @return : the Player who isn't playing
    */
  def getOpponentPlayer() : Player = {
      return this.players.last
    }
}
