package battleship

import org.scalatest._

class ShipSpec extends FlatSpec with Matchers {
  val Ship1 = Ship(1,2,true,"Carrier")
  "The Ship1 object" should "be a Carrier with a size of 5" in {
    Ship1.name shouldEqual "Carrier"
    Ship1.size() shouldEqual 5
  }
  "The Ship1 object" should "be place in 1-2" in {
    Ship1.x shouldEqual 1
    Ship1.y shouldEqual 2
  }
  "The Ship1 object" should "be horizontal" in {
    Ship1.orientation shouldEqual true
  }
}
