package misc

object FlatMapExperiments {


  def main(args: Array[String]): Unit = {
    val list = List(Some(1), Some(2), Some(3), None)
    val newList = list.flatMap { x ⇒ x.map(_ * 2)  }

    val list1 = List(1, 2, 3)
    val bob = for {
      bob     ← list1
    } yield bob

    println(bob)
  }


}
