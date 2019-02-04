package fsis.K

import chapter4.Monad

trait MonadFilter[F[_]] extends Any with Monad[F] {

  def empty[A] : F[A]

  def filter[A](fa: F[A])(fx: A ⇒ Boolean) : F[A] = {
    flatMap(fa)(a ⇒ if (fx(a)) pure(a) else empty)
  }

  def filterM[A](fa: F[A])(fx: A ⇒ F[Boolean]): F[A] = {
    flatMap(fa)(a ⇒ flatMap(fx(a))(b ⇒ if (b) pure(a) else empty[A]) )
  }

}

object MonadFilter {

  def apply[F[_]](implicit M: MonadFilter[F]): MonadFilter[F] = M
}
