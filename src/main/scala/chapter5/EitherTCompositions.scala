package chapter5

import cats.data.{EitherT, OptionT}
import cats.instances.either._
import cats.syntax.applicative._

import scala.concurrent.Future

object EitherTCompositions {

  def main(args: Array[String]): Unit = {

    def myFunc = println("Hello World!")
    val bob : OptionT[Either[Option[String], Option[String]], String] = OptionT(Right("Success"))

  }

}


