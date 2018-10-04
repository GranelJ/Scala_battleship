package battleship

import scala.util.Try
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
    print(s"""Please enter the X coordinate of your ${name} between 1 and 10""")
  }

  /**
    * Function to prompt the message asking for y coordinate
    * @param name : the name of the Ship for which we are asking the coordinate
    */
  def promptAskYCoord(name : String) : Unit = {
    print(s"""Please enter the Y coordinate of your ${name} between 1 and 10""")
  }

  /**
    * Function to prompt the message asking for orientation
    * @param name : the name of the Ship for which we are asking the orientation
    */
  def promptAskOrientationCoord(name : String) : Unit = {
    print(s"""Please enter the orientation coordinate of your ${name}, 'H' for horizontal and 'V' for vertical""")
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
}
