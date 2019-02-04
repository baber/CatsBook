package chapter4.instances

import cats.Id
import chapter4.Applicative

object ApplicativeInstances {

  implicit val applicativeInstanceForId = new Applicative[Id] {
    def pure[A](a: A): Id[A] = a

    def apply[A, B](fa: Id[A])(ff: Id[A ⇒ B]): Id[B] = ff(fa)
  }



}
