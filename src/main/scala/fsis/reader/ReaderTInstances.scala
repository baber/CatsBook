package fsis.reader

import cats.FlatMap

object ReaderTInstances {


  implicit def ReaderTFlatMap[F[_], Z](implicit F: FlatMap[F]) : FlatMap[ReaderT[F, Z, ?]] = {

    new FlatMap[ReaderT[F, Z, ?]] {

      def flatMap[A, B](fa: ReaderT[F, Z, A])(f: A ⇒ ReaderT[F, Z, B]): ReaderT[F, Z, B] = {
        ReaderT(z ⇒ F.flatMap(fa.run(z))(a ⇒ f(a).run(z)))
      }

      def tailRecM[A, B](a: A)(f: A ⇒ ReaderT[F, Z, Either[A, B]]): ReaderT[F, Z, B] = {
        ReaderT(z ⇒ F.tailRecM(a)(f(_).run(z)))
      }

      def map[A, B](fa: ReaderT[F, Z, A])(f: A ⇒ B): ReaderT[F, Z, B] = {
        ReaderT(z ⇒ F.map(fa.run(z))(f))
      }
    }

  }

}
