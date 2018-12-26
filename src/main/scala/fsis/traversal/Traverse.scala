package fsis.traversal

import cats.FlatMap
import chapter2.Functor
import chapter4.Applicative
import chapter7.Foldable

trait Traverse[F[_]] extends Any with Functor[F] with Foldable[F] {

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_] : Applicative, A](fa: F[G[A]]) : G[F[A]] = {
    traverse(fa)(identity)
  }

  def compose[G[_] : Traverse, A] : Traverse[λ[X ⇒ F[G[X]]]] = ???

  def flatTraverse[G[_], A, B](fa: F[A])(f: A => G[F[B]])(implicit G: Applicative[G], F: FlatMap[F]): G[F[B]] = {
    val v1: G[F[F[B]]] = traverse(fa)(f)
    G.map(v1)(a ⇒ F.flatten(a))
  }


  def monadTraverse[G[_] : Applicative, A, B](monad: F[A])(fx: A ⇒ G[B])(implicit T: Traverse[F]): G[F[B]] = {
    T.traverse[G, A, B](monad)(fx)
  }

}


object Traverse {




}

