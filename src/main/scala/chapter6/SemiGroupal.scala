package chapter6

trait SemiGroupal[F[_]] {
  def product[A, B](fa: F[A], fb: F[B]) : F[(A, B)]
}

object SemiGroupal {
  def apply[F[_]](implicit semiGroupal: SemiGroupal[F]): SemiGroupal[F] = semiGroupal
}
