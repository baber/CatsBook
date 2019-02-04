package chapter8

import cats.effect.Sync
import cats.{Applicative, Monoid}

object MapReduceFramework {


  def main(args: Array[String]): Unit = {
    import cats.effect.IO
    import cats.instances.int._

    val bob = runMapReduce[Int, Int, IO](Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 2, _ * 2)

    println(bob.unsafeRunSync())
  }


  def runMapReduce[A, B, F[_] : Sync](data: Seq[A], partitions: Int, mx: A ⇒ B)(implicit M : Monoid[F[B]]): F[B] = {
    val mapped = partition(data, partitions).map {
      seq ⇒ map[F, A, B](seq, mx)
    }

    val reduced = mapped.map {
      fa ⇒ reduce(fa)
    }

    reduced.foldLeft(M.empty)((x, y) ⇒ M.combine(x, y))
  }

  def reduce[F[_], A](seq: Seq[F[A]])(implicit M : Monoid[F[A]], A : Applicative[F]) : F[A] = {
    seq.foldLeft(M.empty)((b, fa) ⇒ M.combine(b, fa))
  }

  def map[F[_] : Sync, A, B](chunk: Seq[A], fx: A ⇒ B): Seq[F[B]] = {
    chunk.map { a ⇒ Sync[F].delay { fx(a) } }
  }

  def partition[A](data: Seq[A], partitions: Int) : Seq[Seq[A]] = {
    data.grouped(partitions).toList
  }


}
