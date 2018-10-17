package misc.whycats

import org.scalatest.Matchers

class MapCombinatorsSpec extends org.scalatest.FreeSpec with Matchers {

  "combine" - {

    "combine should combine two Map[String, List] values with no common keys" in {
      val map1 = Map[String, List[String]]("key1" → List("value1"), "key2" → List("value2"))
      val map2 = Map[String, List[String]]("key3" → List("value3"), "key4" → List("value4"))

      MapCombinators.combineWithCats(map1, map2).toList should contain theSameElementsAs Map(
        "key1" → List("value1"),
        "key2" → List("value2"),
        "key3" → List("value3"),
        "key4" → List("value4")).toList
    }


    "combine should combine two Map[String, List] values with one common key and concatenate the List values" in {
      val map1 = Map[String, List[String]]("key1" → List("value1"), "key2" → List("value2"))
      val map2 = Map[String, List[String]]("key1" → List("value3"))

      MapCombinators.combineWithCats(map1, map2).toList should contain theSameElementsAs Map(
        "key1" → List("value1", "value3"),
        "key2" → List("value2")).toList
    }
  }

  "combineN" - {

    "combineNWithCats should combine three Map[String, List[String]] values with no common keys" in {
      val map1 = Map[String, List[String]]("key1" → List("value1"), "key2" → List("value2"))
      val map2 = Map[String, List[String]]("key3" → List("value3"), "key4" → List("value4"))
      val map3 = Map[String, List[String]]("key5" → List("value5"), "key6" → List("value6"))

      MapCombinators.combineN(map1, map2, map3).toList should contain theSameElementsAs Map(
        "key1" → List("value1"),
        "key2" → List("value2"),
        "key3" → List("value3"),
        "key4" → List("value4"),
        "key5" → List("value5"),
        "key6" → List("value6")).toList
    }

    "combineNWithCats should combine three Map[String, List] values with common keys and concatenate the List values" in {
      val map1 = Map[String, List[String]]("key1" → List("value1"), "key2" → List("value2"))
      val map2 = Map[String, List[String]]("key1" → List("value3"))
      val map3 = Map[String, List[String]]("key2" → List("value4"))

      MapCombinators.combineN(map1, map2, map3).toList should contain theSameElementsAs Map(
        "key1" → List("value1", "value3"),
        "key2" → List("value2", "value4")).toList
    }
  }

}
