package chapter6

import cats.Semigroupal
import cats.data.Validated

object FormValidation {


  case class User(name: String, age: Int)

  def getUser(values: Map[String, String]) = {
    import cats.data.Validated
    import cats.instances.list._
    import cats.syntax.either._

    type AllErrorsOr[A] = Validated[List[String], A]

    Semigroupal[AllErrorsOr].product(readName(values).toValidated, readAge(values).toValidated)
  }


  def readName(values: Map[String, String]): Either[List[String], String] = {
    for {
      name      ← getValue("name", values)
      isValid   ← nonBlank(name)
    } yield isValid

  }

  def readAge(values: Map[String, String]): Either[List[String], Int] = {
    for {
      age             ← getValue("age", values)
      numericalAge    ← parseInt(age)
      isValid         ← nonNegative(numericalAge)
    } yield isValid
  }

  def getValue(key: String, values: Map[String, String]): Either[List[String], String] = {
    Validated.fromOption(values.get(key), List(s"Value not supplied for $key")).toEither
  }

  def parseInt(value: String): Either[List[String], Int] = {
    Validated.catchOnly[NumberFormatException](value.toInt).leftMap(e ⇒ List(s"Invalid number: ${e.getMessage}")).toEither
  }

  def nonBlank(value: String): Either[List[String], String] = {
    import cats.syntax.validated._
    value.valid[List[String]].ensure(List("Value must not be empty"))(!_.isEmpty).toEither
  }

  def nonNegative(value: Int): Either[List[String], Int] = {
    import cats.syntax.validated._
    value.valid[List[String]].ensure(List("Value must be greater than 0"))(_ > 0).toEither
  }


  def main(args: Array[String]): Unit = {
    println(getUser(Map("name1" → "bob", "age1" → "25A")))
  }
}
