package chapter6

import cats.Semigroupal
import chapter6.SemiGroupalInstances._

object SemiGroupalExperiments {

  def main(args: Array[String]): Unit = {
    eitherSemigroupal()
  }

  def optionSemigroupal() = {
    val x = SemiGroupal[Option].product(Some(123), Some("abc"))
    println(x)
  }



  def eitherSemigroupal() = {

    val bob = SemiGroupal[ErrorOr].product(Left(new Throwable("t1")), Left(new Throwable("t2")))
    println(bob)
  }

}
