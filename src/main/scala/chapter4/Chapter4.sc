

trait Mine[A, B, C, Z] {
  def func(a: A)(b: B)(c: C) : Z

  def partialFunc(a: A, b: B) : (A, C) ⇒ Z = func(_)(b)(_)
}


import cats.syntax.either._ // for asRight
val a = 3.asLeft[String]

a map { x ⇒ Right(x + 1)}

def countPositive(nums: List[Int]) =
  nums.foldLeft(Right(0)) { (accumulator, num) =>
    if(num > 0) {
      accumulator.map(_ + 1)
    } else {
      Left("Negative. Stopping!")
    } }