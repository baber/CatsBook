package misc.traversal

import cats.Applicative

object Instances {

  implicit val traverseForList = new Traverse[List] {
    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A ⇒ G[B]): G[List[B]] = {
      fa.foldRight(Applicative[G].pure(List.empty[B]))((a: A, acc: G[List[B]]) ⇒ Applicative[G].map2(f(a), acc)((x, y) ⇒ x :: y))
    }
  }

  implicit val traverseForOption = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A ⇒ G[B]): G[Option[B]] = {
      fa match {
        case Some(a) ⇒ Applicative[G].map(f(a))(Some(_))
        case _ ⇒ Applicative[G].pure(None)
      }
    }
  }

}
