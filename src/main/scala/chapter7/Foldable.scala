package chapter7

import cats.{Eval, Monoid}

trait Foldable[F[_]] extends Any { self ⇒

  def foldLeft[A, B](fa: F[A])(acc: B)(fx: (B, A) ⇒ B) : B

  def foldRight[A, B](fa: F[A])(acc: B)(fx: (A, B) ⇒ B) : B

  def foldRightWithEval[A, B](fa: F[A], acc: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

  def foldMap[A, B](fa: F[A], f: A ⇒ B)(implicit monoid: Monoid[B]) : B = {
    foldLeft(fa)(monoid.empty)((b, a) ⇒ monoid.combine(b, f(a)))
  }

  def combineAll[A](fa: F[A])(implicit M: Monoid[A]) : A = {
    foldLeft(fa)(M.empty)((b, a) ⇒ M.combine(b, a))
  }

  def compose[G[_]](implicit G: Foldable[G]) : Foldable[λ[X ⇒ F[G[X]]]] = new Foldable[λ[X ⇒ F[G[X]]]] {

    def foldLeft[A, B](fga: F[G[A]])(acc: B)(fx: (B, A) ⇒ B): B = {
      self.foldLeft(fga)(acc)((b, ga) ⇒ G.foldLeft(ga)(b)(fx))
    }

    def foldRight[A, B](fga: F[G[A]])(acc: B)(fx: (A, B) ⇒ B): B = {
      self.foldRight(fga)(acc)((ga, b) ⇒ G.foldRight(ga)(b)(fx))
    }

    def foldRightWithEval[A, B](fga: F[G[A]], acc: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = {
      self.foldRightWithEval(fga, acc)((a, b) ⇒ G.foldRightWithEval(a, b)(f))
    }
  }

}

object Foldable {

  def apply[F[_]](implicit F: Foldable[F]) = F

}
