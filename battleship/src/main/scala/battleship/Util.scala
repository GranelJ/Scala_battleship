package battleship

import scala.util.{Random, Try}
import scala.io.StdIn.readLine

object Util {

  /**
    * Function to print the choices at the beginning of the game
    */
  def promptModChoices(): Unit = {
    print(""" Choose the mode :
    1 - Human vs Human
    2 - Human vs Easy AI
    3 - Human vs Medium AI
    4 - Human vs Hard AI
    5 - Easy AI vs Medium AI
    6 - Medium AI vs Hard AI
          """)
  }

  /**
    * Function to get the input of a user when an Int is expected
    * @return : the Int if a user entered one, None otherwise
    */
  def getUserInputInt() : Option[Int] = {
    val input = readLine()
    if(Try(input.toInt).isSuccess){
      return Some(input.toInt)
    }else{
      return None
    }
  }

  /**
    * Function to get the input of a user when a String is expected
    * @return : the String the user entered
    */
  def getUserInputString() : String = {
    return readLine().trim.toUpperCase
  }

  /**
    * Function to prompt the message asking for x coordinate
    * @param name : the name of the Ship for which we are asking the coordinate
    */
  def promptAskXCoord(name : String) : Unit = {
    print(s"Please enter the X coordinate of your ${name} between 0 and 9\n")
  }

  /**
    * Function to prompt the message asking for x coordinate for shot
    */
  def promptAskXCoordShot() : Unit = {
    print(s"Please enter the X coordinate for your shot between 0 and 9\n")
  }

  /**
    * Function to prompt the message asking for y coordinate for shot
    */
  def promptAskYCoordShot() : Unit = {
    print(s"Please enter the Y coordinate for your shot between 0 and 9\n")
  }

  /**
    * Function to prompt the message when shot hasn't hit
    */
  def promptShotMiss() : Unit = {
    println("Shot as miss")
    getUserInputString()
  }

  /**
    * Function to prompt the message when shot hasn't hit
    */
  def promptShotHit() : Unit = {
    println("Shot as hit")
    getUserInputString()
  }

  /**
    * Function to prompt the message when a ship is sunk
    * @param ship : the ship sunkk
    */
  def promptShipSunk(ship : Ship) : Unit = {
    println(s"You sunk the opponent ${ship.name}")
    getUserInputString()
  }

  /**
    * Function to prompt the message asking for y coordinate
    * @param name : the name of the Ship for which we are asking the coordinate
    */
  def promptAskYCoord(name : String) : Unit = {
    print(s"Please enter the Y coordinate of your ${name} between 0 and 9\n")
  }

  /**
    * Function to prompt the message asking for orientation
    * @param name : the name of the Ship for which we are asking the orientation
    */
  def promptAskOrientationCoord(name : String) : Unit = {
    print(s"Please enter the orientation coordinate of your ${name}, 'H' for horizontal and 'V' for vertical\n")
  }

  /**
    * Function to prompt the boards of the player
    * @param player : the player for which we want to prompt the board
    */
  def promptBoards(player : Player) : Unit = {
    print("\nShip Board\n")
    val xCoord = List.range(0,10)
    print("    ")
    xCoord.foreach(x => print(x + " "))
    println()
    print("    - - - - - - - - - -")
    println()
    player.shipBoard.grid.zipWithIndex.foreach{case(list,index) =>{print(index + " | ")
    list.foreach(x => print(x + " "))
    println()}}
    print("\n Shot Board\n")
    print("    ")
    xCoord.foreach(x=>print(x + " "))
    println()
    print("    - - - - - - - - - -")
    println()
    player.shotBoard.grid.zipWithIndex.foreach{case(list,index) =>{print(index + " | ")
    list.foreach(x => print(x + " "))
    println()}}
  }

  /**
    * Function to create the 5 Ship for a Player
    * @param player : the Player to who we want to add ship
    * @return : the new Player with his shipgrid updated
    */
  def create_fleet(player : Player): Player ={
    //If the player is human
    if(player.iA == 0){
      val nameShip = "Destroyer"
      promptAskXCoord(nameShip)
      val inputX = getUserInputInt()
      promptAskYCoord(nameShip)
      val inputY = getUserInputInt()
      promptAskOrientationCoord(nameShip)
      val inputOrientation = getUserInputString()
      val nplayer1 = player.create_Ship(inputX, inputY, inputOrientation, nameShip)
      if (!nplayer1.isEmpty){
        val nameShip = "Submarine"
        promptAskXCoord(nameShip)
        val inputX = getUserInputInt()
        promptAskYCoord(nameShip)
        val inputY = getUserInputInt()
        promptAskOrientationCoord(nameShip)
        val inputOrientation = getUserInputString()
        val nplayer = nplayer1.get.create_Ship(inputX, inputY, inputOrientation, nameShip)
        if (!nplayer.isEmpty){
          val nameShip = "Cruiser"
          promptAskXCoord(nameShip)
          val inputX = getUserInputInt()
          promptAskYCoord(nameShip)
          val inputY = getUserInputInt()
          promptAskOrientationCoord(nameShip)
          val inputOrientation = getUserInputString()
          val nplayer1 = nplayer.get.create_Ship(inputX, inputY, inputOrientation, nameShip)
          if(!nplayer1.isEmpty){
            val nameShip = "Battleship"
            promptAskXCoord(nameShip)
            val inputX = getUserInputInt()
            promptAskYCoord(nameShip)
            val inputY = getUserInputInt()
            promptAskOrientationCoord(nameShip)
            val inputOrientation = getUserInputString()
            val nplayer = nplayer1.get.create_Ship(inputX, inputY, inputOrientation, nameShip)
            if(!nplayer.isEmpty){
              val nameShip = "Carrier"
              promptAskXCoord(nameShip)
              val inputX = getUserInputInt()
              promptAskYCoord(nameShip)
              val inputY = getUserInputInt()
              promptAskOrientationCoord(nameShip)
              val inputOrientation = getUserInputString()
              val nplayer1 = nplayer.get.create_Ship(inputX, inputY, inputOrientation, nameShip)
              if(!nplayer1.isEmpty){
                return nplayer1.get
              }else{
                print("You miss when you create your ship, you must restart\n" +
                  "-------------------------------------------\n")
                return create_fleet(player)
              }
            }else{
              print("You miss when you create your ship, you must restart\n" +
              "-------------------------------------------\n")
              return create_fleet(player)
            }
          }else{
            print("You miss when you create your ship, you must restart\n" +
              "-------------------------------------------\n")
            return create_fleet(player)
          }
        }else{
          print("You miss when you create your ship, you must restart\n" +
            "-------------------------------------------\n")
          return create_fleet(player)
        }
      }else{
        print("You miss when you create your ship, you must restart\n" +
          "-------------------------------------------\n")
        return create_fleet(player)
      }
    }else{
      val OrientList = List("H","V")
      val randX = Random
      val randY = Random
      val randOrient = Random
      var nameShip = "Destroyer"
      var x = randX.nextInt(10)
      var y = randY.nextInt(10)
      var orientation = OrientList(randOrient.nextInt(2))
      val nplayer1 = player.create_Ship(Some(x), Some(y), orientation, nameShip)
      if(!nplayer1.isEmpty){
        x = randX.nextInt(10)
        y = randY.nextInt(10)
        nameShip = "Submarine"
        orientation = OrientList(randOrient.nextInt(2))
        val nplayer2 = nplayer1.get.create_Ship(Some(x), Some(y), orientation, nameShip)
        if(!nplayer2.isEmpty){
          x = randX.nextInt(10)
          y = randY.nextInt(10)
          nameShip = "Cruiser"
          orientation = OrientList(randOrient.nextInt(2))
          val nplayer3 = nplayer2.get.create_Ship(Some(x), Some(y), orientation, nameShip)
          if(!nplayer3.isEmpty){
            x = randX.nextInt(10)
            y = randY.nextInt(10)
            nameShip = "Battleship"
            orientation = OrientList(randOrient.nextInt(2))
            val nplayer4 = nplayer3.get.create_Ship(Some(x), Some(y), orientation, nameShip)
            if(!nplayer4.isEmpty){
              x = randX.nextInt(10)
              y = randY.nextInt(10)
              nameShip = "Carrier"
              orientation = OrientList(randOrient.nextInt(2))
              val nplayerf = nplayer4.get.create_Ship(Some(x), Some(y), orientation, nameShip)
              if(!nplayerf.isEmpty){
                return nplayerf.get
              }else{
                return create_fleet(player)
              }
            }else{
              return create_fleet(player)
            }
          }else{
            return create_fleet(player)
          }
        }else{
          return create_fleet(player)
        }
      }else{
        return create_fleet(player)
      }
    }
  }
}
