package chapter7

import FoldableInstances._
import cats.Eval


object FoldableExperiments {


  def main(args: Array[String]): Unit = {
    println(listFoldRightWithEval().value)
  }

  def listFoldLeft() = {
    val list = List("a", "b", "c")
    Foldable[List].foldLeft(list)("_")(_+_)
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

}
