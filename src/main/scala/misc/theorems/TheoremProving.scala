package misc.theorems

import scala.annotation.implicitNotFound

object TheoremProving {


  def main(args: Array[String]): Unit = {

    val mySeq = Seq(1,2,3,4,5)
    val xs = Seq(1,3)
    println(mySeq.filter(!xs.contains(_)))

//    val myList = new MyList(1, 2, 3)
//    myList.flatten
  }

  @implicitNotFound(msg = "Cannot prove that ${A} =:= ${B}.")
  trait =:=[A, B]
  implicit def isEq[A] : =:=[A, A] = new =:=[A, A] {}

}
