package chapter6

import cats.{Functor, Semigroupal}

object MapNSyntax {

  implicit def bobo[F[_], A0, A1](t2: Tuple2[F[A0], F[A1]]): Tuple2SemigroupalOps[F, A0, A1] = new Tuple2SemigroupalOps(t2)

}


final class Tuple2SemigroupalOps[F[_], A0, A1](t2: Tuple2[F[A0], F[A1]]) {
  def mapN[Z](f: (A0, A1) => Z)(implicit functor: Functor[F], semigroupal: Semigroupal[F]): F[Z] = Semigroupal.map2(t2._1, t2._2)(f)
}
