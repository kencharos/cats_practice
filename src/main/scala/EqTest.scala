import cats.Eq
import cats.Eq._
import cats.implicits._

import scala.concurrent.Future

/**
  * Created by kentaro.maeda on 2017/08/07.
  */
class EqTest extends App {

  case class Hoge(x:String)

  implicit val hogeEq:Eq[Hoge] = new Eq[Hoge] {
    override def eqv(x: Hoge, y: Hoge): Boolean = x.x == y.x
  }

  val h1 = Hoge("End")
  val h2 = Hoge("Start")
  println(h1 === h2)


}
