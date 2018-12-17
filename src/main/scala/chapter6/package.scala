package object chapter6 {
  type ErrorOr[A] = Either[Throwable, A]
}
