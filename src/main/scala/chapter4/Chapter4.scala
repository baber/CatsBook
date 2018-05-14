package chapter4

import chapter2.Functor

object Chapter4 {

  type Id[A] = A


  implicit val optionMonad = new Monad[Option] {

    override def flatMap[A, B](fa: Option[A])(f: A ⇒ Option[B]) = {
      fa match {
        case None ⇒ None
        case Some(a) ⇒ f(a)
      }
    }

    override def pure[A](a: A) = Some(a)
  }



  implicit val identityMonad = new Monad[Id] {

    override def pure[A](a: A) = a

    override def flatMap[A, B](fa: Id[A])(f: A ⇒ Id[B]) = {
      f(fa)
    }
  }



  implicit val identityFunctor = new Functor[Id] {
    override def map[A, B](fa: Id[A])(f: A ⇒ B): Id[B] = {
      f(fa)
    }
  }


  implicit val optionApplicative = new Applicative[Option] {
    override def pure[A](a: A) = Some(a)

    override def apply[A, B](fa: Option[A])(ff: Option[A ⇒ B]) = {
      (fa, ff) match {
        case (None, _) | (_, None) ⇒ None
        case (Some(a), Some(f)) ⇒ Some(f(a))
      }
    }
  }


  implicit val listApplicative = new Applicative[List] {

    override def pure[A](a: A): List[A] = List(a)

    override def apply[A, B](fa: List[A])(ff: List[A ⇒ B]): List[B] = {
      for {
        a   ← fa
        fx  ← ff
      } yield fx(a)
    }
  }

  implicit val listOfOptionsApplicative = listApplicative.compose(optionApplicative)
  implicit val optionOfListApplicative = optionApplicative.compose(listApplicative)


  import Monad.MonadOps

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = for {
    x <- a
    y <- b
  } yield x * x + y * y

  def transform[F[_], A, B](fa: F[A])(fx: A ⇒ B)(implicit applicative: Applicative[F]) = {
    applicative.apply(fa)(applicative.pure(fx))
  }


  def main(args: Array[String]): Unit = {
//    sumSquare(Some(5): Option[Int], Some(10)) foreach(println(_))
//    println(sumSquare(5: Id[Int], 10: Id[Int]) map (_ + 1))
//    println(sumSquare(5: Id[Int], 10: Id[Int]) flatMap (_ + 1))

//    println(optionApplicative.map4WithTuple3(Some(1), Some(2), Some(3), Some(4))(_+_+_+_))
//    println(optionApplicative.map4(Some(1), Some(2), Some(3), Some(4))(_+_+_+_))

    println(transform(Some(10): Option[Int])(a ⇒ s"Hello $a"))
    println(transform(List(1, 2))(a ⇒ s"Hello $a"))
    println(transform(List(Some(1), Some(2)))(a ⇒ s"Hello $a"))
    println(transform(Some(List(1, 2)): Option[List[Int]])(a ⇒ s"Hello $a"))

    println(listOfOptionsApplicative.apply(List(Some(1): Option[Int], Some(2), None))(List(
                                                                                          Some(a ⇒ s"Hello $a"),
                                                                                          Some(a ⇒ s"Goodbye $a")): List[Option[Int ⇒ String]]))

  }





}
