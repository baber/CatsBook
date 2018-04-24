package chapter4

import chapter2.Functor

trait Monad[F[_]] {

  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A ⇒ F[B]): F[B]

  def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = {
    flatMap(fa)(f andThen pure)
  }

}

object Monad {

  implicit class MonadOps[A, F[_]](x: F[A]) {

    def map[B](f: A ⇒ B)(implicit m:  Monad[F]) : F[B] = {
      m.map(x)(f)
    }

    def flatMap[B](f: A ⇒ F[B])(implicit m: Monad[F]) : F[B] = {
      m.flatMap(x)(f)
    }

  }

}
