package chapter5

import cats.Monoid
import cats.data.Writer
import cats.data.OptionT
import cats.implicits._

object LocalTransformation {


  type Logged[A] = Writer[List[String], A]

  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None => Writer(List(s"Failed on $str"), None)
    }



  def addAll(a: String, b: String, c: String)(implicit m: Monoid[Option[Int]]): Logged[Option[Int]] = {

    val result = for {
      a <- parseNumber(a)
      b <- parseNumber(b)
      c <- parseNumber(c)
    } yield m.combineAll(List(a, b, c))

    result

//    val result = for {
//      a <- Some(1)
//      b <- Some(1)
//      c <- Some(1)
//    } yield a + b + c


//    Writer(List(), result)
  }


  def main(args: Array[String]): Unit = {
    val x = addAll("2", "55", "100")
    println(x.run)
  }

}
