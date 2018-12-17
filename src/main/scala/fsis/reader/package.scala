package fsis

import chapter4.Monad

package object reader {

  type Id[A] = A

  object Id {

    implicit val idMonad: Monad[Id] = new Monad[Id] {
      def pure[A](a: A): Id[A] = a

      def flatMap[A, B](fa: Id[A])(f: A ⇒ Id[B]): Id[B] = f(fa)
    }

  }

  type Reader[A, B] = ReaderT[Id, A, B]

}
