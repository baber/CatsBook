package chapter4.syntax

import chapter4.Applicative

object ApplicativeSyntax {

  implicit def toApplicativeOps[F[_], A](a: F[A]): ApplicativeOps[F, A] = new ApplicativeOps[F, A](a)

  final class ApplicativeOps[F[_], A](value: F[A]) {

    def map[B](fx: A ⇒ B)(implicit F: Applicative[F]): F[B] = {
      F.map(value)(fx)
    }

  }


}
