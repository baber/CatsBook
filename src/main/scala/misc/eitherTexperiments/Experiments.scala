package misc.eitherTexperiments

import cats.data.EitherT
import cats.effect.IO
import cats.syntax.either._


sealed trait Failure extends Product with Serializable
sealed trait TextFailure extends Failure


object Experiments {

  def funcA() : IO[Either[Failure, String]] = ???

  def funcB() : IO[Either[TextFailure, String]] = ???

  def main(args: Array[String]): Unit = {

    for {
      a         ← EitherT(funcB)
      b         ← EitherT(funcA)
    } yield b

  }

}
