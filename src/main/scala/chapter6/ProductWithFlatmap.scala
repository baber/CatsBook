package chapter6

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.instances.option._

object ProductWithFlatmap {

  def product[M[_] : Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = {
    for {
      a     ← x
      b     ← y
    } yield (a,b)
  }

  def sortWithImplicits[T](xs: Seq[T])(implicit ordering: Ordering[T]) = {
    xs.sorted
  }

  def sort[T : Ordering](xs: Seq[T]): Seq[T] = {
    xs.sorted
  }


  def main(args: Array[String]): Unit = {
    val list = List(1, 3, 5, 4, 2)
    println(sort(list))
//    product(Some(1) : Option[Int], Some(2))
  }


}
