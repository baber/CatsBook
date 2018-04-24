import cats.Functor

object Chapter2 {

  implicit def function1Functor[X]: Functor[X ⇒ ?] = new Functor[X ⇒ ?] {
    override def map[A, B](fa: X ⇒ A)(f: A ⇒ B): X => B = {
      fa andThen f
    }
  }

  def main(args: Array[String]) : Unit = {
    m1
    m2
  }

  def m1() = {

    def myFunc1 = (x: Int) => s"Hello $x"
    def myFunc2 = (x: String) => s"What did you say? $x"
    def myFunc3 = (x: Int) => x + x

    val myMappedFunc = function1Functor.map(myFunc3)(myFunc1)
    println(myMappedFunc(100))
  }


  def m2() = {

    implicit class FunctorOps[F[_], A](src: F[A])  {
      def map[B](fx: A ⇒ B)(implicit fnc: Functor[F]) = {
        fnc.map(src)(fx)
      }
    }

    def myFunc1 = (x: Int) => s"Hello $x"
    def myFunc2 = (x: String) => s"What did you say? $x"
    def myFunc3 = (x: Int) => x + x

    val myMappedFunc = myFunc1.map(myFunc2)
    println(myMappedFunc(200))

  }

}
