package misc.kleisli

import cats.data.ReaderT
import cats.effect.IO

object KleisliExperiments {

  def functorK[A, B](readerT: ReaderT[IO, A, B]) : IO[A ⇒ B] = ???

}
