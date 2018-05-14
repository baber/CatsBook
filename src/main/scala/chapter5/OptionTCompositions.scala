package chapter5

import cats.Monad
import cats.data.OptionT
import cats.instances.list._
import cats.syntax.applicative._ // for pure
import cats.instances.either._


object OptionTCompositions {

  type ListOption[A] = OptionT[List, A]

  def main(args: Array[String]): Unit = {
    optionWithList()
    optionWithEither()
  }

  def optionWithList() = {
    val result1: ListOption[Int] = OptionT(List(Option(10)))
    val result2: ListOption[Int] = 32.pure[ListOption]
    val result3 = OptionT(List(Option(20), Option(30)))

    println(
      for {
        x ← result1
        y ← result3
      } yield x + y
    )

  }


  def optionWithEither() = {
    val a = 10.pure[OptionT[Either[String, ?], ?]]
    val b = 32.pure[OptionT[Either[String, ?], ?]]
    val c = "hello".pure[OptionT[Either[String, ?], ?]]

    val result = for {
      x ← a
      y ← b
      z ← c
    } yield x + y + z

    println(result)
  }

}

