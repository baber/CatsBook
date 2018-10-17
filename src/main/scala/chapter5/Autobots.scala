package chapter5

import cats.data.EitherT

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import cats.implicits._

import scala.concurrent.duration._

object Autobots {

  type Response[A] = EitherT[Future, String, A]
  val powerLevels = Map(
    "Jazz" → 6,
    "Bumblebee" → 8,
    "Hot Rod" → 100)

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot).fold(
      EitherT(Future(Left(s"Power level for autobot $autobot could not be found!"): Either[String, Int])))(x ⇒ EitherT(Future(Right(x): Either[String, Int])))
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      p1          ← getPowerLevel(ally1)
      p2          ← getPowerLevel(ally2)
    } yield (p1 + p2) > 15
  }


  def tacticalReport(ally1: String, ally2: String): String = {
     Await.result(canSpecialMove(ally1, ally2).fold(x ⇒ x, x ⇒ if (x) "Got the power!" else "Need food...so hungry!"), 1 second)
  }

  def main(args: Array[String]): Unit = {
    println(tacticalReport("Jazz", "Bumblebee"))
    println(tacticalReport("Bumblebee", "Hot Rod"))
    println(tacticalReport("Jazz", "Ironhide"))

//    val x = for {
//      optimusPower ← getPowerLevel("Jazz")
//      megatronPower ← getPowerLevel("Bumblebee")
//    } yield optimusPower + megatronPower
//
//    println(Await.result(x.value, 1.second))
  }


}
