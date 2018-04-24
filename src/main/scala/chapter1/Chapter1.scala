import cats.Show
//import cats.Show

//import cats._
//import cats.data._
//import cats.implicits._


object Chapter1 {

  import java.util.Date
  //  implicit val dateShow: Show[Date] =
  //    new Show[Date] {
  //      override def show(date: Date): String =
  //        s"${date.getTime}ms since the epoch."
  //    }


  def doIt() = {
//        import cats._
//    import cats.implicits._
    import cats.syntax.show._
    //    import cats.instances._

    //val showInt: Show[Int] = cats.Show.apply[Int]
    //val showString: Show[String] = Show.apply[String]

    //123.show
    //"abc".show


    implicit val dateShow: Show[Date] = Show.show(date => s"${date.getTime}ms since the epoch.")

    val date = new Date(2018, 1, 18)
    println(dateShow.show(date))
    println(date.show)
    //    println("123".show)
    //    println(date.show)
  }


  def main(args: Array[String]): Unit = {
    doIt()
  }

}
