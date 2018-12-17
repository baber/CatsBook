package chapter7

import cats.Eval

trait Foldable[F[_]] {

  def foldLeft[A, B](fa: F[A])(acc: B)(fx: (B, A) ⇒ B) : B

  def foldRight[A, B](fa: F[A])(acc: B)(fx: (A, B) ⇒ B) : B

  def foldRightWithEval[A, B](fa: F[A], acc: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

}

object Foldable {

  def apply[F[_]](implicit F: Foldable[F]) = F

}
