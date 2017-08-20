import cats.Eq
import cats.Eq._
import cats.implicits._

import scala.concurrent.Future

/**
  * Created by kentaro.maeda on 2017/08/07.
  */
object EqTest extends App {


  val a = if (true) "aaa" else 45

  println(a)

}
