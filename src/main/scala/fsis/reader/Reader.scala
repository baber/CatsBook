package fsis.reader

import chapter4.Monad
import fsis.reader.Id._


object Reader {

  def apply[A, B](f: A ⇒ B) : Reader[A, B] =
    ReaderT[Id, A, B](f)

  def ask[A] : Reader[A, A] = {
    Reader(identity)
  }

  implicit def readerMonad[R] : Monad[Reader[R, ?]]  = new Monad[Reader[R, ?]] {
    def pure[A](a: A): Reader[R, A] = Reader { _ ⇒ a }

    def flatMap[B, C](fa: Reader[R, B])(f: B ⇒ Reader[R, C]): Reader[R, C] = {
      fa.flatMap(f)
    }
  }


  def main(args: Array[String]): Unit = {

    type StringReader[A] = Reader[String, A]

    val rev : StringReader[String] = Reader(_.reverse)
    val upper : StringReader[String] = Reader(_.toUpperCase)

    val both = rev flatMap(a ⇒ Reader(_ ⇒ a.toUpperCase))

    println(both.run("baber"))
  }
}
