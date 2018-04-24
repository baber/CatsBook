package chapter3

trait Contravariant[F[_]] {

  def contramap[A, B](fa: F[A])(f: B ⇒ A) : F[B]

}



