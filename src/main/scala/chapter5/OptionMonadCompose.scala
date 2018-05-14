package chapter5

import cats.Applicative


object OptionMonadCompose {

  import cats.Monad
  import cats.syntax.applicative._ // for pure
  import cats.syntax.flatMap._     // for flatMap
  import scala.language.higherKinds


  def compose[M1[_] : Monad, M2 : Option]()  = {

    type Composed[A] = M1[Option[A]]

    new Monad[Composed] {

      def pure[A](a: A): Composed[A] =
        Option(a).pure[M1]

      def flatMap[A, B](fa: M1[Option[A]])(f: A => M1[Option[B]]): M1[Option[B]] = {
        val F = implicitly[Monad[M1]]

        fa.flatMap(_.fold(F.pure[Option[B]](None))(f))
      }

      override def tailRecM[A, B](a: A)(f: A ⇒ Composed[Either[A, B]]): Composed[B] = ???
    }

  }

}




