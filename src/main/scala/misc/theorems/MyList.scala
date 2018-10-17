package misc.theorems

import TheoremProving.=:=

class MyList[A](a: A*) {

  def flatten[B](implicit ev : =:=[A, List[B]]) : MyList[B] = ???

}

object MyList {
  def apply[A](a: A*): MyList[A] = new MyList(a:_*)
}
