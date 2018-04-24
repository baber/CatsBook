package chapter2

import Wrapper._

object Main {

  def main(args: Array[String]): Unit = {
    val wrapper = Wrapper(45)

    wrapperFunctor.map(wrapper)(_ + 5)
  }


}

