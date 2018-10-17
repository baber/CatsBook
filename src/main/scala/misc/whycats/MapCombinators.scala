package misc.whycats

import cats.Monoid

object MapCombinators {

  def combine[A, B](map1: Map[A, List[B]], map2: Map[A, List[B]]) : Map[A, List[B]] = {

    def combineStep(mp: Map[A, List[B]], tp: (A, List[B])) : Map[A, List[B]] = {
      mp + (tp._1 → (mp.getOrElse[List[B]](tp._1, List[B]()) ++ tp._2))
    }

    (map1.toList ++ map2.toList).foldLeft[Map[A, List[B]]](Map[A, List[B]]())(combineStep)
  }

  def combineN[A, B](maps: Map[A, List[B]]*) : Map[A, List[B]] = {
    maps.foldLeft(Map.empty[A, List[B]])(combine)
  }

  def combineNWithCats[A, B](maps: Map[A, List[B]]*) : Map[A, List[B]] = {
    import cats.instances.map._
    import cats.instances.list._
    implicitly[Monoid[Map[A, List[B]]]].combineAll(maps)
  }

  def combineWithCats[A, B](map1: Map[A, List[B]], map2: Map[A, List[B]]) : Map[A, List[B]] = {
    import cats.instances.map._
    import cats.instances.list._
    import cats.syntax.monoid._
    map1 |+| map2
  }


  def main(args: Array[String]): Unit = {
    val map1 = Map("k1" -> List("One"), "k2" -> List("Zero"))
    val map2 = Map("k1" -> List("Two"))
    
    println(combine(map1, map2))
  }

}
