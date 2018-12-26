package misc

import org.scalatest.{FlatSpec, Matchers}

object ListProblems {

  def runLengths[A](list: List[A]): Int = {

    def runLengthsInternal(list: List[A], elements: List[A], acc: Int): Int = {
      list match {
        case x :: Nil ⇒ if (elements.size > 1) acc + 1 else acc
        case x :: xs ⇒ {
          elements.headOption.fold(runLengthsInternal(xs, List(x), acc)) {
            a ⇒
              if (a == x) {
                runLengthsInternal(xs, x :: elements, acc)
              } else {
                if (elements.size > 1) runLengthsInternal(xs, List(), acc + 1) else {
                  runLengthsInternal(xs, List(x), acc)
                }
              }
          }
        }
      }
    }

    runLengthsInternal(list, List(), 0)
  }


  def collectRun[A](list: List[A]) : (List[A], List[A]) = {
    list.headOption.fold((list, List[A]()))(
      a ⇒ foldLeftUntil(list, List[A]())((acc, a) ⇒ a :: acc)(_ == a)
    )
  }


  def foldLeftUntil[A, B](fa: List[A], acc: B)(fx: (B, A) ⇒ B)(fy: A ⇒ Boolean): (List[A], B) = {
    fa match {
      case x::Nil   ⇒ if (fy(x)) (List(), fx(acc, x)) else (List(x), acc)
      case x::xs    ⇒ if (fy(x)) foldLeftUntil(xs, fx(acc, x))(fx)(fy) else (fa, acc)
    }
  }

}


class ListProblemsSpec extends FlatSpec with Matchers {

  import ListProblems._

  "collectRun" should "return an single element list if there is no run at the beginning of a list" in {
    val list = List("a", "b")
    collectRun(list) should be ((List("b"), List("a")))
  }

  it should "return a run of two elements when given a list with a run of two elements at the beginning" in {
    val list = List("a", "a", "b")
    collectRun(list) should be (List("b"), List("a", "a"))
  }


  "runLengths" should "return 1 for a list with a single run" in {

    val list = List("a", "b", "b", "c")
    runLengths(list) should be (1)
  }

  it should "return 2 for a list with two runs" in {
    val list = List("a", "b", "b", "c", "d", "d", "e", "f")
    runLengths(list) should be (2)
  }


}
