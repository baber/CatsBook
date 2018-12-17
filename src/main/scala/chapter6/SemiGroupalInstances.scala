package chapter6


object SemiGroupalInstances {

  implicit def optionSemiGroupal = new SemiGroupal[Option] {
    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = {
      (fa, fb) match {
        case (Some(a), Some(b)) ⇒ Some((a,b))
        case _ ⇒ None
      }
    }
  }

  implicit def eitherSemiGroupal = new SemiGroupal[ErrorOr] {
    override def product[A, B](fa: ErrorOr[A], fb: ErrorOr[B]): ErrorOr[(A, B)] = {
      (fa, fb) match {
        case (Right(a), Right(b)) ⇒ Right((a, b))
        case (Left(a), _) ⇒ Left(a)
        case (_, Left(a)) ⇒ Left(a)
      }
    }
  }


}
