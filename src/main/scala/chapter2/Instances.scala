package chapter2

import chapter4.Monad

object Instances {

  implicit val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: A ⇒ B): List[B] = fa.map(f)
  }

  implicit val listMonad = new Monad[List] {

    def pure[A](a: A): List[A] = List(a)

    def flatMap[A, B](fa: List[A])(f: A ⇒ List[B]): List[B] = fa.flatMap(f)
  }

}
