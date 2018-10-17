package misc.traversal

import cats.{Applicative, FlatMap}

trait Traverse[F[_]] {

  def flatTraverse[G[_], A, B](fa: F[A])(f: A => G[F[B]])(implicit G: Applicative[G], F: FlatMap[F]): G[F[B]] = {
    val v1: G[F[F[B]]] = traverse(fa)(f)
    G.map(v1)(a ⇒ F.flatten(a))
  }

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]


  def listTraverse[F[_] : Applicative, A, B](list: List[A])(fx: A ⇒ F[B]): F[List[B]] = {
    list.foldRight(Applicative[F].pure(List.empty[B]))((a: A, acc: F[List[B]]) ⇒ Applicative[F].map2(fx(a), acc)((x, y) ⇒ x :: y))
  }

  def monadTraverse[F[_], G[_] : Applicative, A, B](monad: F[A])(fx: A ⇒ G[B])(implicit T: Traverse[F]): G[F[B]] = {
    T.traverse[G, A, B](monad)(fx)
  }


  def main(args: Array[String]): Unit = {
    //    val myList = List(1, 2, 3)
    //    val traversedList = monadTraverse[List, Option, Int, String](myList)(x ⇒ Some(s"Hello $x"))
    //    println(traversedList)
    //
    //    val myOption = Some(1)
    //    val traversedOption = monadTraverse[Option, List, Int, String](myOption)(x ⇒ List(s"Hello $x"))
    //    println(traversedOption)


  }
}

