package fsis.K

object SemigroupKInstances {


  implicit val listSemigroupKInstance = new SemigroupK[List] {

    def combine[A](x: List[A], y: ⇒ List[A]): List[A] = {
      x ++ y
    }
  }

  implicit val optionSemigroupKInstance = new SemigroupK[Option] {

    def combine[A](x: Option[A], y: ⇒ Option[A]): Option[A] = x orElse y
  }


}
