package misc.whycats

import scala.util.Try

object Bob extends cats.syntax.FunctorSyntax

object NestedStructures {


  def mapNestedTry(nested: List[Try[Int]]) : List[Try[Int]] = {
    nested.map { _.map(_+1) }
  }

  def mapNestedTryWithCats(nested: List[Try[Int]]) : List[Try[Int]] = {
    import cats.syntax.functor._
    import cats.instances.try_._
    import cats.instances.list._
    import cats.data.Nested.catsDataTraverseForNested
    import cats.data.Nested

    Nested(nested).map(_ + 1).value
  }


}
