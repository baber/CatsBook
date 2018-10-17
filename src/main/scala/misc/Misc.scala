package misc

import cats.Applicative
import cats.instances.option._
import cats.instances.list._

object Misc {

  trait Traverse[F[_]] {
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
  }

  implicit val traverseForList = new Traverse[List] {
    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A ⇒ G[B]): G[List[B]] =  {
      fa.foldRight(Applicative[G].pure(List.empty[B]))((a : A, acc: G[List[B]]) ⇒  Applicative[G].map2(f(a), acc)((x, y) ⇒ x :: y)   )
    }
  }

  implicit val traverseForOption = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A ⇒ G[B]): G[Option[B]] = {
      fa match {
        case Some(a) ⇒ Applicative[G].map(f(a))(Some(_))
        case _ ⇒ Applicative[G].pure(None)
      }
    }
  }

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(fx: A ⇒ F[B]) : F[List[B]] = {
    list.foldRight(Applicative[F].pure(List.empty[B]))((a : A, acc: F[List[B]]) ⇒  Applicative[F].map2(fx(a), acc)((x, y) ⇒ x :: y)   )
  }

  def monadTraverse[F[_], G[_] : Applicative, A, B](monad: F[A])(fx: A ⇒ G[B])(implicit T : Traverse[F]) : G[F[B]] = {
    T.traverse[G, A, B](monad)(fx)
  }


  def main(args: Array[String]): Unit = {
//    val myList = List(1, 2, 3)
//    val traversedList = monadTraverse[List, Option, Int, String](myList)(x ⇒ Some(s"Hello $x"))
//    println(traversedList)
//
//    val myOption = Some(1)
//    val traversedOption = monadTraverse[Option, List, Int, String](myOption)(x ⇒ List(s"Hello $x"))
//    println(traversedOption)

    println(forComprehensions())

  }



  def forComprehensions() = {
    for {
      x       ← Some(5)
      y       ← Some(10)
      z       ← getAnOption(x)
    } yield x + y + z
  }


  def getAnOption(x: Int) = Some(x)

}



