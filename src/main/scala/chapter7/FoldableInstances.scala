package chapter7
import cats.Eval

object FoldableInstances {

  implicit def ListFoldable: Foldable[List] = new Foldable[List] {

    def foldLeft[A, B](fa: List[A])(acc: B)(fx: (B, A) ⇒ B): B = {
      fa.foldLeft(acc)(fx)
    }


    def foldRight[A, B](fa: List[A])(acc: B)(fx: (A, B) ⇒ B): B = {
      fa match {
        case x :: Nil ⇒ fx(x, acc)
        case x :: xs  ⇒ fx(x, foldRight(xs)(acc)(fx))
      }
    }

    def foldRightWithEval[A, B](fa: List[A], acc: Eval[B])(fx: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = {
      fa match {
        case x::Nil           ⇒ fx(x, acc)
        case x::xs            ⇒ fx(x, acc.flatMap(b ⇒ foldRightWithEval(xs, Eval.now(b))(fx)))
      }
    }

  }

  implicit def OptionFoldable: Foldable[Option] = new Foldable[Option] {

    def foldLeft[A, B](fa: Option[A])(acc: B)(fx: (B, A) ⇒ B): B = {
      fa match {
        case Some(a) ⇒ fx(acc, a)
        case _ ⇒ acc
      }
    }

    def foldRight[A, B](fa: Option[A])(acc: B)(fx: (A, B) ⇒ B): B = {
      fa match {
        case Some(a)  ⇒ fx(a, acc)
        case _        ⇒ acc
      }
    }

    def foldRightWithEval[A, B](fa: Option[A], acc: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] = {
      fa match {
        case Some(a)  ⇒ f(a, acc)
        case _        ⇒ acc
      }
    }

  }
}
