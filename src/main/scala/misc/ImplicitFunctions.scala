package misc

object ImplicitFunctions {


  trait Ord[T] {
    def compare(a: T, b: T): Boolean
  }

  def comp[T](x: T, y: T)(implicit ev: Ord[T]): Boolean = ev.compare(x, y)

  implicit def intOrd: Ord[Int] = new Ord[Int] { def compare(a: Int, b: Int): Boolean = a < b
  }


  def main(args: Array[String]): Unit = {
    println(comp(1, 2))
  }


}
