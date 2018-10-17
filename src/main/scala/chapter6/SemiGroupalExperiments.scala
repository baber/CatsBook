package chapter6

import chapter6.SemiGroupalInstances._

import scala.concurrent.Future

object SemiGroupalExperiments {

  def main(args: Array[String]): Unit = {
    val x = SemiGroupal[Option].product(Some(123), Some("abc"))
    println(x)
  }

}
