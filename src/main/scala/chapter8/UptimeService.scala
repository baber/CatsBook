package chapter8

import fsis.traversal.Traverse
import fsis.traversal.Instances._
import chapter4.Applicative
import chapter4.syntax.ApplicativeSyntax._

class UptimeService[F[_] : Applicative](client: UptimeClient[F]) {

  def getTotalUptime(hostnames: List[String]): F[Int] =
    Traverse[List].traverse(hostnames)(client.getUptime).map(_.sum)
}
