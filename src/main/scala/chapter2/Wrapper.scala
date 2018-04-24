package chapter2

case class Wrapper[A](x: A)


object Wrapper {

  implicit val wrapperFunctor = new Functor[Wrapper] {
    override def map[A, B](fa: Wrapper[A])(f: A ⇒ B) = {
      new Wrapper(f(fa.x))
    }
  }

}