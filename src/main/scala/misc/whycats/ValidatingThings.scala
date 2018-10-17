package misc.whycats

object ValidatingThings {

  def isEven(value: Int): Either[String, Int] = {
    if (value % 2 == 0) {
      Right(value)
    } else {
      Left(s"$value is not even")
    }
  }


    def validateEven(input: List[Int])(f: Int => Either[String, Int]): Either[String, List[Int]] = {
      input.dropWhile(x ⇒ isEven(x).isRight) match {
        case (Nil)  ⇒ Right(input)
        case (x::_) ⇒ Left(s"$x is not even")
      }

    }




}
