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


}
