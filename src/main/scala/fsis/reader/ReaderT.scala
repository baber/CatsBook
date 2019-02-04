package fsis.reader

import cats.FlatMap
import chapter2.Functor
import chapter4.{Applicative, Monad}


case class ReaderT[F[_], A, B](run: A ⇒ F[B]) {

  def map[C](f: B ⇒ C)(implicit F : Functor[F]) : ReaderT[F, A, C] = {
    ReaderT(a ⇒ F.map(run(a))(f))
  }

//  def flatMap[C](f: B ⇒ F[C])(M : Monad[F]) : ReaderT[F, A, C] = {
//    ReaderT(a ⇒ M.flatMap(run(a))(f))
//  }

  def flatMap[C](f: B ⇒ ReaderT[F, A, C])(implicit M : Monad[F]) : ReaderT[F, A, C] = {
    ReaderT(a ⇒  M.flatMap(run(a))(b ⇒ f(b).run(a)))
  }

  def compose[C](r: ReaderT[F, C, A])(implicit F: FlatMap[F]): ReaderT[F, C, B] = {
    ReaderT(c ⇒ F.flatMap(r.run(c))(run))
  }

  def andThen[C](r: ReaderT[F, B, C])(implicit F: FlatMap[F]): ReaderT[F, A, C] = {
    ReaderT(a ⇒ F.flatMap(run(a))(r.run))
  }


}

object ReaderT {

  def ask[F[_], A]()(implicit A : Applicative[F]) : ReaderT[F, A, A] = {
    ReaderT(A.pure)
  }

  def liftF[F[_], A, B](fb: F[B]) : ReaderT[F, A, B] = {
    ReaderT(_ ⇒ fb)
  }

  implicit def ReaderTMonad[F[_], Z, ?]()(implicit M : Monad[F]) : Monad[ReaderT[F, Z, ?]] = new Monad[ReaderT[F, Z, ?]] {
    def pure[A](a: A): ReaderT[F, Z, A] = ReaderT(z ⇒ M.pure(a))

    def flatMap[A, B](fa: ReaderT[F, Z, A])(f: A ⇒ ReaderT[F, Z, B]): ReaderT[F, Z, B] = {
      ReaderT(z ⇒ fa.flatMap(f).run(z))
    }
  }

}
