
final case class Cat(name: String, age: Int, color: String)

trait Printable[A] {

  def format(x: A): String

}


object PrintableInstances {


  implicit val StringPrintable = new Printable[String] {
    override def format(x: String) = x
  }

  implicit val IntPrintable = new Printable[Int] {
    override def format(x: Int) = x.toString
  }

  implicit val CatPrintable = new Printable[Cat] {
    override def format(x: Cat) = s"${x.name} is a ${x.age} year-old ${x.color} cat."
  }

}


object Printable {

  def format[A](x: A)(implicit p: Printable[A]) = {
    p.format(x)
  }

  def print[A](x: A)(implicit p: Printable[A]): Unit = {
    println(p.format(x))
  }

}


//import PrintableInstances._
//Printable.print(Cat("Tom", 10, "Black & White"))


object PrintableSyntax {

  implicit class PrintableOps[A](x: A) {

    def format(implicit printable: Printable[A]): String = {
      printable.format(x)
    }

    def print(implicit printable: Printable[A]): Unit = {
      println(printable.format(x))
    }

  }

}


//import PrintableInstances._
//import PrintableSyntax._
//Cat("Tom", 10, "Black & White").print

import cats._
import cats.implicits._
import cats.Show
//import cats.syntax.show._

val showInt: Show[Int] = cats.Show.apply[Int]
val showString: Show[String] = Show.apply[String]

123.show
"abc".show

implicit val catShow: Show[Cat] = Show.show(x => s"${x.name} is a ${x.age} year-old ${x.color} cat.")
Cat("Tom", 10, "Black & White").show


