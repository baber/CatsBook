package fsis.eitherT

import chapter2.Functor

object EitherTExamples {

  def main(args: Array[String]): Unit = {
    println(eitherTMonadExample())
  }

  def eitherTFunctorCompositionExample(): EitherT[List, String, List[Int]] = {
    import chapter2.Instances.listFunctor
    import fsis.eitherT.EitherT.eitherTFunctor
    val composedFunctor = Functor[EitherT[List, String, ?]].compose(Functor[List])

    val stringOrIntses: List[Either[String, List[Int]]] = List(Right(List(1, 2, 3)), Left("failure"))
    val value: EitherT[List, String, List[Int]] = EitherT(stringOrIntses)

    composedFunctor.map(value)(_ * 2)
  }

  def eitherTMonadExample(): EitherT[List, String, Int] = {

    import chapter2.Instances.listMonad

    val xs : List[Either[String, Int]] = List(Right(1), Right(2), Right(3))
    val ys : List[Either[String, Int]] = List(Right(4), Right(5))

    for {
      x     ← EitherT(xs)
      y     ← EitherT(ys)
    } yield x + y

  }

}
