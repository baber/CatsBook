package fsis.K

import cats.MonoidK
import chapter7.Foldable

trait MonadCombine[F[_]] extends MonadFilter[F] with MonoidK[F] {

  def unite[G[_] : Foldable, A](fga: F[G[A]]) : F[A] = {
    flatMap(fga)(ga ⇒ Foldable[G].foldMap[A, F[A]](ga, a ⇒ pure(a))(algebra[A]))
  }

}
