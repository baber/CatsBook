package fsis.traversal

import cats.FlatMap
import chapter2.Functor
import chapter4.Applicative
import chapter7.Foldable

trait Traverse[F[_]] extends Any with Functor[F] with Foldable[F] { self ⇒

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_] : Applicative, A](fa: F[G[A]]) : G[F[A]] = {
    traverse(fa)(identity)
  }

  def compose[G[_]: Traverse, A] : Traverse[λ[X ⇒ F[G[X]]]] = ???

  def flatTraverse[G[_], A, B](fa: F[A])(f: A => G[F[B]])(implicit G: Applicative[G], F: FlatMap[F]): G[F[B]] = {
    val v1: G[F[F[B]]] = traverse(fa)(f)
    G.map(v1)(a ⇒ F.flatten(a))
  }


  def monadTraverse[G[_] : Applicative, A, B](monad: F[A])(fx: A ⇒ G[B])(implicit T: Traverse[F]): G[F[B]] = {
    T.traverse[G, A, B](monad)(fx)
  }

}


object Traverse {

  def apply[F[_]](implicit F: Traverse[F]): Traverse[F] = F


  trait Composite[F[_], G[_]] extends Any with Traverse[λ[X ⇒ F[G[X]]]] {

    def F: Traverse[F]
    def G: Traverse[G]

    def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A ⇒ H[B]) : H[F[G[B]]] = {
      F.traverse(fga)(ga ⇒ G.traverse(ga)(f))
    }
  }

}

