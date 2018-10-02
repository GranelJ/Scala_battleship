package battleship

//Orientation true means horizontal and false vertical
case class Ship (x : Int, y : Int, orientation : Boolean, name : String){

    //function to get the size of the ship according to his name
    def size() : Int = {
        return this.name match {
            case "Carrier" => 5
            case "Battleship" => 4
            case "Cruiser" => 3
            case "Submarine" => 3
            case "Destroyer" => 2
        }
    }

    //function to get the number of the ship according to his name
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
    //TODO
    def is_Sink() : Boolean = {

    }**/
}

