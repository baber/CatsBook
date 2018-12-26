package fsis.traversal

import cats.Eval
import chapter4.Applicative

object Instances {

  implicit val traverseForList = new Traverse[List] {

    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A ⇒ G[B]): G[List[B]] = {
      fa.foldRight(Applicative[G].pure(List.empty[B]))((a: A, acc: G[List[B]]) ⇒ Applicative[G].map2(f(a), acc)((x, y) ⇒ x :: y))
    }

    def map[A, B](fa: List[A])(f: A ⇒ B): List[B] = fa.map(f)

    def foldLeft[A, B](fa: List[A])(acc: B)(fx: (B, A) ⇒ B): B = fa.foldLeft(acc)(fx)

    def foldRight[A, B](fa: List[A])(acc: B)(fx: (A, B) ⇒ B): B = fa.foldRight(acc)(fx)

    def foldRightWithEval[A, B](fa: List[A], acc: Eval[B])(fx: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = {
      fa match {
        case x::Nil           ⇒ fx(x, acc)
        case x::xs            ⇒ fx(x, acc.flatMap(b ⇒ foldRightWithEval(xs, Eval.now(b))(fx)))
      }
    }
  }

  implicit val traverseForOption = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A ⇒ G[B]): G[Option[B]] = {
      fa match {
        case Some(a) ⇒ Applicative[G].map(f(a))(Some(_))
        case _ ⇒ Applicative[G].pure(None)
      }
    }

    def map[A, B](fa: Option[A])(f: A ⇒ B): Option[B] = ???

    def foldLeft[A, B](fa: Option[A])(acc: B)(fx: (B, A) ⇒ B): B = ???

    def foldRight[A, B](fa: Option[A])(acc: B)(fx: (A, B) ⇒ B): B = ???

    def foldRightWithEval[A, B](fa: Option[A], acc: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = ???
  }

}
