package fsis.K

import cats.Semigroup

trait SemigroupK[F[_]] extends Any { self ⇒

  def combine[A](x: F[A], y: ⇒ F[A]) : F[A]

  def toSemigroup[A]: Semigroup[F[A]] = (x: F[A], y: F[A]) => combine(x, y)

}

object SemigroupK {

  def apply[F[_]](implicit SG: SemigroupK[F]): SemigroupK[F] = SG


}
