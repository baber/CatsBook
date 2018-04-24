package chapter3

final case class Box[A](value: A)


object Main {

  trait Printable[A]  { self ⇒

    def format(x: A): String

    def contramap[B](f: B ⇒ A): Printable[B] = {
      new Printable[B] {
        override def format(x: B) = self.format(f(x))
      }
    }

  }


  implicit val stringPrintable = new Printable[String] {
    override def format(x: String): String = x
  }

  implicit val booleanPrintable = new Printable[Boolean] {
    override def format(x: Boolean): String = x.toString
  }

  implicit val boxPrintableForString = stringPrintable.contramap((x: Box[String]) ⇒ x.value)
  implicit val boxPrintableForBoolean = booleanPrintable.contramap((x: Box[Boolean]) ⇒ x.value)

}


object Printable {

  import Main._

  def format[A](x: A)(implicit p: Printable[A]) = {
    p.format(x)
  }

  def print[A](x: A)(implicit p: Printable[A]): Unit = {
    println(p.format(x))
  }

}


object Test {

  import Printable._

  def main(args: Array[String]): Unit = {
    println(format(Box(true)))
  }
}
