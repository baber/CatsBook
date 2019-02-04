package fsis.K

object MonadFilterInstances {

  implicit val listMonadFilter = new MonadFilter[List] {
    def empty[A]: List[A] = List[A]()

    def pure[A](a: A): List[A] = List(a)

    def flatMap[A, B](fa: List[A])(f: A ⇒ List[B]): List[B] = {
      fa.flatMap(f)
    }
  }


  def main(args: Array[String]): Unit = {
    val list = List("jim", "bob", "barry", "fred", "beatrice")
    println(MonadFilter[List].filter(list)(_.startsWith("b")))
  }

}
