package chapter8


trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}
