package fsis.eitherT

import chapter2.Functor
import chapter4.Monad

case class EitherT[F[_], L, R](value: F[Either[L, R]]) { self ⇒

  def flatMap[B](f: R ⇒ EitherT[F, L, B])(implicit F: Monad[F], M: Monad[EitherT[F, L, ?]]) : EitherT[F, L, B] = {
    M.flatMap(self)(f)
  }

  def map[B](f: R ⇒ B)(implicit F: Monad[F], M: Monad[EitherT[F, L, ?]]) : EitherT[F, L, B] = {
    M.map(self)(f)
  }

}


object EitherT {

  def apply[F[_], L, R](value: F[Either[L, R]]) : EitherT[F[?], L, R] = new EitherT(value)


  implicit def eitherTFunctor[F[_], L](implicit F: Functor[F]) : Functor[EitherT[F, L, ?]] = new Functor[EitherT[F, L, ?]] {
    def map[A, B](fa: EitherT[F, L, A])(f: A ⇒ B): EitherT[F, L, B] = {
      EitherT(F.map(fa.value)(e ⇒ e.map(f)))
    }
  }

  implicit def eitherTMonad[F[_], L](implicit M : Monad[F]) : Monad[EitherT[F, L, ?]] = new Monad[EitherT[F, L, ?]] {
    def pure[A](a: A): EitherT[F, L, A] = EitherT(M.pure(Right(a)))

    def flatMap[A, B](fa: EitherT[F, L, A])(f: A ⇒ EitherT[F, L, B]): EitherT[F, L, B] = {
      EitherT(M.flatMap(fa.value)(e ⇒ e.fold(x ⇒ M.pure[Either[L, B]](Left(x)), a ⇒ f(a).value)))
    }
  }


}
