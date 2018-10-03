package battleship

case class GameState(players : Set[Player]) {

      def getActivePlayer() : Player = {
        return this.players.head
      }

      def getOpponentPlayer() : Player = {
        return this.players.last
      }
}
