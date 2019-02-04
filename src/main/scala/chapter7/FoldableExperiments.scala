package chapter7

import FoldableInstances._
import cats.{Eval, Monoid}


object FoldableExperiments {


  def main(args: Array[String]): Unit = {
    import cats.instances.int._

    println(listOflistsFoldLeft())
    val list = List(List("a", "b"), List("c", "d"), List("e", "f"))
    println(composedListListFoldLeft(list, "")((b, a) ⇒ s"$b $a"))

//    val list = List(Some(1), Some(2), Some(3))
//    println(composedListOptionFoldLeft(list, "")((b, a) ⇒ s"$b $a"))

//    println(listFoldMap(List(1, 2, 3), (x: Int) ⇒ x*2))
//    println(listCombineAll(List(1, 2, 3)))
//    println(listFoldRightWithEval().value)
  }

  def listFoldLeft() = {
    val list = List("a", "b", "c")
    Foldable[List].foldLeft(list)("_")(_+_)
  }

  def listOflistsFoldLeft() = {
    val list = List(List("a", "b"), List("c", "d"), List("e", "f"))
    Foldable[List].foldLeft(list)("")((b, a) ⇒ s"$b $a")
  }

  def listFoldRight() = {
//    val list = List("a", "b", "c")
    val list = (1 to 100000).toList
    Foldable[List].foldRight(list)(0)(_ + _)
  }

  def listFoldRightWithEval() = {
//        val list = List("a", "b", "c")
    val list = (1 to 100000).toList

    Foldable[List].foldRightWithEval(list, Eval.now(0))((a, b) ⇒ b.map(x ⇒ a + x))
  }

  def optionExperiments() = {
    val option = Option(123)

    Foldable[Option].foldLeft(option)(10)(_ * _)
  }

  def listCombineAll[A](list: List[A])(implicit M : Monoid[A]) : A = {
    Foldable[List].combineAll(list)
  }

  def listFoldMap[A, B](list: List[A], f: A ⇒ B)(implicit M : Monoid[B]) : B = {
    Foldable[List].foldMap(list, f)
  }

  def composedListOptionFoldLeft[A, B](list: List[Option[A]], acc: B)(fx: (B, A) ⇒ B) : B = {
    val composedFoldable = Foldable[List] compose Foldable[Option]
    composedFoldable.foldLeft(list)(acc)(fx)
  }

  def composedListListFoldLeft[A, B](list: List[List[A]], acc: B)(fx: (B, A) ⇒ B) : B = {
    val composedFoldable = Foldable[List] compose Foldable[List]
    composedFoldable.foldLeft(list)(acc)(fx)
  }


}
