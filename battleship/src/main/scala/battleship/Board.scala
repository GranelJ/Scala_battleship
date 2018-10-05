package battleship


case class Board (grid : List[List[Int]] = List.fill(10)(List.fill(10)(0))){

  /**
    * Function to check if the ship can be placed on the board
    * @param ship : the ship to check
    * @return : true if it can be placed otherwise false
    */
  def check_Ship(ship: Ship): Boolean = {
        val ship_size = ship.size()
        //If ship horizontal
        if(ship.orientation){
            if(ship.x + ship.size > 10){
                return false
            }else{
                val shipCoord = List.range(ship.x, ship.x + ship.size())
                //return true if the ship doesn't overpass
              return !shipOverpass(ship, shipCoord, false)
            }
        //Ship vertical
        }else{
            if(ship.y + ship.size >10){
                return false
            }else{
                val shipCoord = List.range(ship.y, ship.y + ship.size())
                //return true if the ship doesn't overpass
              return !shipOverpass(ship, shipCoord, false)
            }
        }
  }

  /**
    * Function to know of a ship overpass on another
    * @param ship      : the Ship to test
    * @param shipCoord : the List of x coordinates for horizontal ship, the List of y coordinates for vertical ship
    * @param overPass  : the Boolean to know if the ship overpass
    * @return : true if a ship overpass on another, false otherwise
    */
  def shipOverpass(ship: Ship, shipCoord: List[Int], overPass: Boolean): Boolean = {
    if (shipCoord.isEmpty || overPass) {
            return overPass
        }else{
            if(ship.orientation){
                val nOverpass = this.grid(ship.y)(shipCoord.head) > 0
              shipOverpass(ship, shipCoord.tail, nOverpass)
            }else{
                val nOverpass = this.grid(shipCoord.head)(ship.x) > 0
              shipOverpass(ship, shipCoord.tail, nOverpass)
            }
        }
    }

  /**
    * Function to create the List of coordinate that will be added to the grid
    * @param shiplist  : the List of Int which will be added in the Board grid
    * @param shipCoord : the List of x coordinates of the ship to add
    * @param ship      : the ship to add
    * @return : a List of Int which have the number of the ship at his x coordinates
    */
  def create_Ship_List_Horizontal(shiplist: List[Int], shipCoord: List[Int], ship: Ship): List[Int] = {
        if (shipCoord.size == 0){
            return shiplist
        }else{
            val nshiplist = shiplist.updated(shipCoord.head, ship.number())
            val nshipCoord = shipCoord.tail
          create_Ship_List_Horizontal(nshiplist, nshipCoord, ship)
        }
  }

  /**
    * Function to place an horizontal ship on the board
    * @param ship : the Ship to add
    * @return : the new Board with the number of the ship at his coordinates
    */
  def add_Ship(ship: Ship): Board = {
        if(ship.orientation){
            //list of the coordinate of the ship
            val shipCoord = List.range(ship.x, ship.x + ship.size())
            //list that will be updated and then placed in the grid
            val tempShipList = this.grid(ship.y)
            //the updated list
            val shiplist = create_Ship_List_Horizontal(tempShipList, shipCoord, ship)
            //update the grid with the new list
            val nGrid = this.grid.updated(ship.y, shiplist)
            return Board(nGrid)
        }else{
            val tempShiplist = this.grid.slice(ship.y, ship.y + ship.size())
            //get the list of list to update the grid
            val shiplist = create_Ship_List_Vertical(tempShiplist, 0, ship)
            //create the updated grid
            val ngrid = add_Vertical_Ship_In_Grid(grid, shiplist, ship)
            return Board(ngrid)
        }

  }
  /**
    * Function to create the list of list with the number of the ship at his coordinates
    * @param shiplist : the List of List of Int which will be filled with the ship's number where it's necessary
    * @param iterator : the iterator if
    * @param ship     : the Ship to place
    * @return : the List of List of Int to add to the Board grid
    */
  def create_Ship_List_Vertical(shiplist: List[List[Int]], iterator: Int, ship: Ship): List[List[Int]] = {
        if(iterator == ship.size()){
            return shiplist
        }else{
            val list = shiplist(iterator)
            val tempList = list.updated(ship.x, ship.number())
            val nshiplist = shiplist.updated(iterator, tempList)
          create_Ship_List_Vertical(nshiplist, iterator + 1, ship)
        }
  }

  /**
    * Function to add a vertical ship to the grid
    * @param grid     : a List of List of Int that will be updated with the List from shipList
    * @param shiplist : a List of List of Int which has the ship's number at y ship coordinates
    * @param ship     : the Ship to add
    * @return : return a List of List of Int that will be used to create a new board
    */
  def add_Vertical_Ship_In_Grid(grid: List[List[Int]], shiplist: List[List[Int]], ship: Ship): List[List[Int]] = {
        if(shiplist.size == 0){
            return grid
        }else{
            //update the grid with the new line with the last list of the shiplist
            val nGrid = grid.updated(ship.y + shiplist.size-1, shiplist.last)
            val nshiplist = shiplist.init
          add_Vertical_Ship_In_Grid(nGrid, nshiplist, ship)
        }
  }

  /**
    * Function to know if a shot as hit a ship or not
    * @param xShot : x coordinate of the shot
    * @param yShot : y coordinate of the shot
    * @return : true if the shot as hit a ship, otherwise false
    */
  def shot_As_Hit(xShot: Int, yShot: Int): Boolean = {
        if(this.grid(yShot)(xShot) > 0) {
            return true
        }else{
            return false
        }
  }

  /**
    * Function to update the grid after a shot with -1 at shot coordinates
    * @param xShot : x coordinate of the shot
    * @param yShot : y coordinate of the shot
    * @return : the new Board updated with -1 at the shot coordinates
    */
  def update_Grid_After_Shot(xShot: Int, yShot: Int): Board = {
        //create new list to update grid
        val nList = this.grid(yShot).updated(xShot, -1)
        val nGrid = this.grid.updated(yShot, nList)
        return Board(nGrid)
  }

  /**
    * Function to know if there still is ship on the board
    * @return : true if there still is ship on the board, false otherwise
    */
  def as_Ship(): Boolean = {
    //flat the grid to have one list with all the elements
    val flattenGrid = this.grid.flatten
    //filter to get the list of element > 0
    val as_Ship = flattenGrid.filter(x => x > 0)
    return !as_Ship.isEmpty
    }

  /**
    * Function to get the value of a coordinate
    * @param x : x coordinate to evaluate
    * @param y : y coordinate to evaluate
    * @return : the value at x,y in the grid
    */
  def getValGrid(x: Int, y : Int) : Int = {
    val valCoord = this.grid(y)(x)
    return valCoord
  }
}