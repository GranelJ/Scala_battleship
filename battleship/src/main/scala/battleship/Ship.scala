package battleship

//Orientation true means horizontal and false vertical
case class Ship (x : Int, y : Int, orientation : Boolean, name : String){

    /**
      * Function to get the size of the ship according to his name
      * @return : the size of the ship
      */
    def size() : Int = {
        return this.name match {
            case "Carrier" => 5
            case "Battleship" => 4
            case "Cruiser" => 3
            case "Submarine" => 3
            case "Destroyer" => 2
        }
    }

    /**
      * Function to get the number of the ship according to his name
      * @return : the number of the ship
      */
    def number() : Int = {
        return this.name match {
            case "Carrier" => 5
            case "Battleship" => 4
            case "Cruiser" => 3
            case "Submarine" => 2
            case "Destroyer" => 1
        }
    }

    /**
      * Function to know if a ship is sunk
      * @param board : the board where it needs to check
      * @return : true if the ship is sunk, otherwise false
      */
    def is_Sunk(board : Board) : Boolean = {
        val flattenBoard = board.grid.flatten
        val is_Sunk = flattenBoard.filter(x =>  x == this.number())
        return is_Sunk.isEmpty
    }
}

