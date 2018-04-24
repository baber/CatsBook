package chapter4

import chapter2.Functor

import scala.language.existentials

trait Applicative[F[_]] extends Functor[F] { self ⇒

  def pure[A](a: A) : F[A]

  def apply[A, B](fa: F[A])(ff: F[A ⇒ B]) : F[B]

  def apply2[A, B, Z](fa: F[A], fb: F[B])(ff: F[(A,B) ⇒ Z]) : F[Z] = {
    apply(fa)(apply(fb)(map(ff)(f ⇒ b ⇒ a ⇒ f(a,b))))
  }

  override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = {
    apply(fa)(pure(f))
  }


  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) ⇒ Z) : F[Z] = {
    apply(fa)(map(fb)(b ⇒ f(_, b)))
  }

  def tuple2[A, B, Z](fa: F[A], fb: F[B]) : F[(A,B)] = {
    map2(fa, fb)((_,_))
  }

  def map3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) ⇒ Z) : F[Z] = {
    apply(fa)(map2(fb, fc)((b, c) ⇒ f(_, b, c)))
  }

  def tuple3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C]) : F[(A, B, C)] = {
    map3(fa, fb, fc)((_, _, _))
  }

  def map4WithTuple3[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) ⇒ Z) : F[Z] = {
    map2(tuple3(fa, fb, fc), fd)((a, b) ⇒ f(a._1, a._2, a._3, b))
  }


  def map4[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) ⇒ Z) : F[Z] = {
    apply(fa)(map3(fb, fc, fd)((b, c, d) ⇒ f(_, b, c, d)))
  }




  def compose[G[_]](implicit G: Applicative[G]) : Applicative[λ[X ⇒ F[G[X]]]] = new Applicative[λ[X ⇒ F[G[X]]]] {

    override def pure[A](a: A): F[G[A]] = {
      self.pure(G.pure(a))
    }

    override def apply[A, B](fga: F[G[A]])(ff: F[G[A ⇒ B]]): F[G[B]] = {
      self.apply(fga)(self.map(ff)(gab ⇒ ga ⇒ G.apply(ga)(gab)))
    }
  }



}
