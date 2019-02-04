package chapter4


trait Monad[F[_]] extends Any with Applicative[F] {

  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A ⇒ F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = {
    flatMap(fa)(f andThen pure)
  }

  override def product[A, B](fa: F[A], fb: F[B]) : F[(A, B)] = {
    flatMap(fa)(a ⇒ flatMap(fb)(b ⇒ pure((a, b))))
  }

  override def apply[A, B](fa: F[A])(f: F[A ⇒ B]) : F[B] = {
    flatMap(fa)(a ⇒ flatMap(f)(fx ⇒ pure(fx(a))))
  }

  def alternativeApply[A, B](fa: F[A], f: F[A ⇒ B]) : F[B] = {
    flatMap(f)(fx ⇒ map(fa)(a ⇒ fx(a)))
  }

  def flatten[A](ffa : F[F[A]]) : F[A] = {
    flatMap(ffa)(identity)
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
