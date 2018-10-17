package chapter6

object SyntaxExperiments {


  def main(args: Array[String]): Unit = {
    import chapter6.MapNSyntax._
    import cats.instances.option._
    val bob = (Some(1): Option[Int], Some(2)).mapN(_+_)
    println(bob)
  }

}
