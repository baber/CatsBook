package chapter2


trait Functor[F[_]] extends Any { self ⇒

  def map[A, B](fa: F[A])(f: A ⇒ B) : F[B]

  def lift[A, B](f: A ⇒ B) : F[A] ⇒ F[B] = {
    fa ⇒ map(fa)(f)
  }

  def as[A, B](fa: F[A])(b: ⇒ B): F[B] = {
    map(fa)(_ ⇒ b)
  }

  def void[A, B](fa: F[A]) : F[Unit] = {
    map(fa)(_ ⇒ Unit)
  }



  def compose[G[_]](implicit G: Functor[G]) : Functor[λ[X ⇒ F[G[X]]]] = {
    new Functor[λ[X ⇒ F[G[X]]]] {
      def map[A, B](fga: F[G[A]])(f: A ⇒ B) : F[G[B]] = {
        self.map(fga)(ga ⇒ G.map(ga)(a ⇒ f(a)))
      }
    }
  }

//  def compose[G[_]](implicit G: Functor[G]) : F[G[?]] = ???

}
