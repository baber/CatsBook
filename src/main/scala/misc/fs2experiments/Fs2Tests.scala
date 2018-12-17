package misc.fs2experiments

import java.util.concurrent.ForkJoinPool

import cats.effect.{IO, Timer}
import fs2.Pure

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.io.Source

object Fs2Tests {


  val ec: ExecutionContext                = ExecutionContext.fromExecutor(new ForkJoinPool(sys.runtime.availableProcessors()))
  implicit val timer: Timer[IO]           = IO.timer(ec)

  val positiveInts = fs2.Stream.unfold(1)(x ⇒ Some((x, x+1)))
  val random = fs2.Stream.randomSeeded(scala.util.Random.nextLong)

  def usingUnfold() = {
    fs2.Stream.unfold(1)(s ⇒ Some((s, s*2)))
  }

  def usingUnfoldChunk() = {
    fs2.Stream.unfoldEval[IO, Int, Int](1)(s ⇒ IO(Some((s, s*2))))
  }

  def usingFlatMap[A](stream: fs2.Stream[Pure, A]) = {
    stream.flatMap(x ⇒ fs2.Stream.eval[IO, A](IO(x)))
  }

  def sleepStream() = {
    fs2.Stream.sleep[IO](2 seconds).repeat
  }


//  def main(args: Array[String]): Unit = {
//    import cats.instances.string._
//    val stream = fs2.Stream.awakeEvery[IO](1 second).map(_.toString).take(5).to[IO](fs2.Sink.showLinesStdOut[IO, String])
//    stream.compile.drain.unsafeRunSync()
//  }


//  def main(args: Array[String]): Unit = {
//    val bob = Source.fromFile("/Users/baberkhalil/Downloads/failed_jobs_notes.txt").getLines().toList
//    val jim: Seq[Int] = bob.map(x ⇒ Stream.iterate(x.toInt)(_+1).take(6)).flatten
//    val k =  jim.map(_.toString).fold[String]("")(_ + " " + _)
//    println(s"( $k )")
//    println(s"( $k )")
//  }


//def main(args: Array[String]): Unit = {
//  val bob = Source.fromFile("/Users/baberkhalil/Downloads/baber.txt").getLines().toList
//  val jim: Seq[Int] = bob.map(x ⇒ x.toInt + 5)
//  val k =  jim.map(_.toString).fold[String]("")(_ + " " + _)
//  println(s"( $k )")
//  println(s"( $k )")
//}

  def main(args: Array[String]): Unit = {

    val myList = Stream.iterate((1,8))(tup ⇒ (tup._2+1, tup._2+8)).take(115).toList


    println("is=(" + myList.map(_._1.toString).fold("")((a,b) ⇒ s"$a $b") + ")")
    println("js=(" + myList.map(_._2.toString).fold("")((a,b) ⇒ s"$a $b") + ")")

//    println((1 to 912 by 6).map(_.toString).fold("")((a, b) ⇒ a + " " + b))
//    println((6 to 912 by 7).map(_.toString).fold("")((a, b) ⇒ a + " " + b))
  }
}
