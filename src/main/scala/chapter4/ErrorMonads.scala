package chapter4


object ErrorMonads {

  trait MonadError[F[_], E]  {

    def pure[A](a: A) : F[A]

    def raiseError[A](e: E) : F[A]

    def handleError[A](fa: F[A])(fx: E ⇒ A) : F[A]

    def ensure[A](fa: F[A])(e: E)(fx: A ⇒ Boolean) : F[A]

  }


  type ErrorOr[A] = Either[String, A]

  val monadError = new MonadError[ErrorOr, String] { self ⇒

    override def pure[A](a: A): ErrorOr[A] = Right(a)

    override def raiseError[A](e: String): ErrorOr[A] = {
      Left(e)
    }

    override def handleError[A](fa: ErrorOr[A])(fx: String ⇒ A): ErrorOr[A] = {
        fa match {
          case Left(msg) if "Not too bad".equalsIgnoreCase(msg) ⇒ pure(fx(msg))
          case _ ⇒ raiseError("I can't recover from that")
        }
    }

    override def ensure[A](fa: ErrorOr[A])(e: String)(fx: A ⇒ Boolean): ErrorOr[A] = {
      fa flatMap {
        a ⇒ if (fx(a)) pure(a) else raiseError(e)
      }

    }
  }

}
