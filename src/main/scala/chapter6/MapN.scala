package chapter6

import cats.{Functor, Semigroupal}
import cats.instances.option._

object MapN {

  def map2[F[_], A, B, Z](fab: (F[A], F[B]))(fx: (A, B) ⇒ Z)(implicit FNC: Functor[F], SG: Semigroupal[F]) : F[Z] = {
    FNC.map(SG.product(fab._1, fab._2))(y ⇒ fx(y._1, y._2))
  }


  def main(args: Array[String]): Unit = {
    val bob = map2[Option, Int, Int, Int]((Some(1), Some(2)))(_+_)
    println(bob)
  }
}


