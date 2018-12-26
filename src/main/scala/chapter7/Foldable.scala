package chapter7

import cats.{Eval, Monoid}

trait Foldable[F[_]] extends Any {

  def foldLeft[A, B](fa: F[A])(acc: B)(fx: (B, A) ⇒ B) : B

  def foldRight[A, B](fa: F[A])(acc: B)(fx: (A, B) ⇒ B) : B

  def foldRightWithEval[A, B](fa: F[A], acc: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

  def foldMap[A, B](fa: F[A], f: A ⇒ B)(implicit monoid: Monoid[B]) : B = {
    foldLeft(fa)(monoid.empty)((b, a) ⇒ monoid.combine(b, f(a)))
  }

}

object Foldable {

  def apply[F[_]](implicit F: Foldable[F]) = F

}
