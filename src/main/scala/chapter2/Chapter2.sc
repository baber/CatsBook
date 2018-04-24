import cats.{Functor, Monoid}
import cats.implicits._
import cats.instances.function._ // for Functor
import cats.syntax.functor._ // for map


def add(items: List[Int]): Int = {
  items.foldLeft(0)(_ + _)
}

add(List(1, 2, 3, 4))

def typeVariantAdd[A](items: List[A], identity: A)(f: (A, A) => A): A = {
  items.foldLeft(identity)((x, y) => f(x, y))
}

def combineOptions(a: Option[Int], b: Option[Int]) = {
  (a, b) match {
    case (None, Some(x)) => Some(x)
    case (Some(x), None) => Some(x)
    case (None, None) => None
    case (Some(x), Some(y)) => Some(x + y)

  }
}

typeVariantAdd(List(Some(1), Some(2), Some(3), Some(4), None), None)(combineOptions)


def monoidBasedAdd[A](items: List[A])(implicit monoid: Monoid[A]) = {
  items.foldLeft(monoid.empty)((x, y) => monoid.combine(x, y))
}

monoidBasedAdd(List(1, 2, 3, 4))
monoidBasedAdd(List(Some(1), Some(2), Some(3), Some(4), None))


val func1: Int => Double =
  (x: Int) => x.toDouble

val func2: Double => Double =
  (y: Double) => y * 2


case class Wrapper[A](a: A) {
  def getIt: A = a
}


implicit val myFunctor1 = new Functor[Wrapper] {
  override def map[A, B](fa: Wrapper[A])(f: A => B): Wrapper[B] = {
    Wrapper(f(fa.getIt))
  }
}



def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] = {
  start.map(_ * 2)
}


doMath(List(1, 2, 3))
doMath(Some(5): Option[Int])
doMath(Wrapper(50))

def functorLift[A, B, F[_]](fx: A => B)(implicit functor: Functor[F]): (F[A]) => F[B] = {
  def myFunc(fa: F[A]): F[B] = {
    fa.map(fx)
  }

  myFunc
}

def monoidalCombine[A](x: A)(implicit monoid: Monoid[A]): A = {
  monoid.combine(x, x)
}


val square = (x: Int) => {
  x * x
}
val liftedSquare = functorLift[Int, Int, Option](square)
liftedSquare(Some(6))

val sayHelloToANumber = (x: Int) => {
  s"Hello $x"
}

val liftedSayHelloToANumber = functorLift[Int, String, Option](sayHelloToANumber)
liftedSayHelloToANumber(Some(100))

val liftedCombineIntsFunction = functorLift[Int, Int, Wrapper](monoidalCombine[Int])
liftedCombineIntsFunction(Wrapper(5))

val liftedCombineStringsFunction = functorLift[String, String, Wrapper](monoidalCombine[String])
liftedCombineStringsFunction(Wrapper("Hello"))


import cats.instances.function._ // for Functor
import cats.syntax.functor._ // for map


type F[A] = Int => A


implicit val f1 = Functor.apply[F]


//implicit class Functors[F[A], A, B](x: F[A]) {
//  def map(fx: A ⇒ B)(implicit fnc: Functor[F]) = {
//    fnc.map(x)(fx)
//  }
//}


implicit def function1Functor[X] : Functor[X ⇒ ?] = new Functor[X ⇒ ?] {
  override def map[A, B](fa: X ⇒ A)(f: A ⇒ B) : X => B = {
    fa andThen f
  }
}

//implicit val function1Functor = new Functor[F] {
//  override def map[A, B](fa: F[A])(f: A => B) = {
//    fa andThen f
//  }
//}

def myFunc1 = (x: Int) => s"Hello $x"
def myFunc2 = (x: String) => s"What did you say? $x"

//val myMappedFunc = f1.map(myFunc1)(myFunc2)
val myMappedFunc = function1Functor.map(myFunc1)(myFunc2)

myMappedFunc(50)

//val squareAndThenSayHello = square.map(sayHelloToANumber)


//def makeAFunctor[F[_]] = {
//  Functor.apply[F]
//}
//
//implicit val myFunctor2 = makeAFunctor[Wrapper]
//
//def myFunc(a: Int) : String = "hello " + a
//val myLiftedFunc = myFunctor1.lift(myFunc)
//
//myLiftedFunc(Wrapper(1))
//
//Wrapper("bob").map(_ + " jim")


//implicit val myFunctor2 = Functor[(Double) => Double]


//(func1 map func2)(1)