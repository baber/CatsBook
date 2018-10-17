package misc.whycats

import org.scalatest.{FreeSpec, Matchers}

import scala.util.{Failure, Success, Try}

class NestedStructuresSpec extends FreeSpec with Matchers {

  "nested structures" - {
    "mapNested should map nested Try[Int] types successfully" in {

      val throwable = new Throwable("Not valid")
      val nested: List[Try[Int]] = List (
        Success(1),
        Success(2),
        Failure(throwable),
        Success(3)
      )

      NestedStructures.mapNestedTryWithCats(nested) should contain theSameElementsAs List(
        Success(2),
        Success(3),
        Failure(throwable),
        Success(4)
      )
    }

  }
}
