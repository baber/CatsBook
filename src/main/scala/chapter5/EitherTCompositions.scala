package chapter5

import cats.data.{EitherT, OptionT}
import cats.instances.either._
import cats.syntax.applicative._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global

object EitherTCompositions {

  def main(args: Array[String]): Unit = {

    // Future[Either[Option]]
    type FutureEitherOption[A] = OptionT[EitherT[Future, String, ?], A]


    val z: OptionT[EitherT[Future, String, ?], Int] = for {
      x ← 10.pure[FutureEitherOption]
      y ← 20.pure[FutureEitherOption]
    } yield x + y

    val one: EitherT[Future, String, Option[Int]] = z.value
    val two: Future[Either[String, Option[Int]]] = z.value.value

    println(Await.result(two, 1 second))

  }

}


