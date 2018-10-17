package misc.whycats

import org.scalatest.{FreeSpec, Matchers}
import scala.util.Right

class ValidatingThingsSpec extends FreeSpec with Matchers {

  "validating things" - {
    "validate even for list should return the list if all elements are even numbers" in {
      ValidatingThings.validateEven(List(2, 4, 6, 8))(ValidatingThings.isEven) shouldBe Right(List(2, 4, 6, 8))
    }

    "validate even for list should return a Left with the first invalid element in a list with odd numbers" in {
      ValidatingThings.validateEven(List(2, 4, 5, 8))(ValidatingThings.isEven) shouldBe Left("5 is not even")
    }
  }
}
