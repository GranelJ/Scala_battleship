package battleship


case class Board (grid : List[List[Int]]){

    //Function to check if the ship can be placed on the board, true if it can be placed otherwise false
    def Check_Ship(ship : Ship) : Boolean = {
        val ship_size = ship.size()
        //If ship horizontal
        if(ship.orientation){
            if(ship.x + ship.size > 10){
                return false
            }else{
                val shipCoord = List.range(ship.x, ship.x + ship.size())
                //return true if the ship doesn't overpass
                return !ShipOverpass(ship, shipCoord, false)
            }
        //Ship vertical
        }else{
            if(ship.y + ship.size >10){
                return false
            }else{
                val shipCoord = List.range(ship.y, ship.y + ship.size())
                //return true if the ship doesn't overpass
                return !ShipOverpass(ship, shipCoord, false)
            }
        }
    }

    //Function that returns true if a ship overpass on another
    def ShipOverpass(ship : Ship, shipCoord : List [Int], overPass : Boolean): Boolean = {
        if(shipCoord.size == 0 || overPass){
            return overPass
        }else{
            if(ship.orientation){
                val nOverpass = this.grid(ship.y)(shipCoord.head) > 0
                ShipOverpass(ship, shipCoord.tail, nOverpass)
            }else{
                val nOverpass = this.grid(shipCoord.head)(ship.x) > 0
                ShipOverpass(ship, shipCoord.tail, nOverpass)
            }
        }
    }

    /**
    @tailrec
    **/
    //Function to create the list of coordinate that will be added to the grid
    def Create_ship_list_horizontal(shiplist : List[Int], shipCoord : List[Int], ship : Ship) : List[Int] = {
        if (shipCoord.size == 0){
            return shiplist
        }else{
            val nshiplist = shiplist.updated(shipCoord.head, ship.number())
            val nshipCoord = shipCoord.tail
            Create_ship_list_horizontal(nshiplist, nshipCoord, ship)
        }
    }

    //Function to place an horizontal ship on the board
    def Add_ship(ship : Ship) : Board = {
        if(ship.orientation){
            //list of the coordinate of the ship
            val shipCoord = List.range(ship.x, ship.x + ship.size())
            //list that will be updated and then placed in the grid
            val tempShipList = List.fill(10)(0)
            //the updated list
            val shiplist = Create_ship_list_horizontal(tempShipList, shipCoord, ship)
            //update the grid with the new list
            val nGrid = this.grid.updated(ship.y, shiplist)
            return Board(nGrid)
        }else{
            val tempShiplist = List.fill(ship.size())(List.fill(10)(0))
            //get the list of list to update the grid
            val shiplist = Create_ship_list_vertical(tempShiplist, 0, ship)
            //create the updated grid
            val ngrid = Add_vetical_ship_in_grid(grid, shiplist, ship)
            return Board(ngrid)
        }
        
    }

    //Function to create the list of list with the number of the ship at his coordinates
    def Create_ship_list_vertical(shiplist : List[List[Int]], iterator : Int, ship : Ship) : List[List[Int]] = {
        if(iterator == ship.size()){
            return shiplist
        }else{
            val list = List.fill(10)(0)
            val tempList = list.updated(ship.x, ship.number())
            val nshiplist = shiplist.updated(iterator, tempList)
            Create_ship_list_vertical(nshiplist, iterator + 1, ship)
        }
    }

    def Add_vetical_ship_in_grid(grid : List[List[Int]], shiplist : List[List[Int]], ship : Ship) : List[List[Int]] = {
        if(shiplist.size == 0){
            return grid
        }else{
            //update the grid with the new line with the last list of the shiplist
            val nGrid = grid.updated(ship.y + shiplist.size-1, shiplist(shiplist.size-1))
            val nshiplist = shiplist.init
            Add_vetical_ship_in_grid(nGrid, nshiplist, ship)
        }
    }

    //Function to know if a shot as hit a ship or not
    def Shot_As_Hit(xShot : Int, yShot : Int) : Boolean = {
        if(this.grid(yShot)(xShot) > 0) {
            return true
        }else{
            return false
        }
    }
}