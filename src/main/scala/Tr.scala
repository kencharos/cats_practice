/**
  * Created by kentaro.maeda on 2017/08/08.
  */
import cats.instances.list._
import cats.instances.option._
import cats.syntax.traverse._
import cats.data._
import cats.implicits._

object Tr extends App{

  val list = List(Some(1), Some(2), None)
  // list: List[Option[Int]] = List(Some(1), Some(2), None)
 // val a = List(1, 2, 3) traverse { (x: Int) => (Some(x + 1): Option[Int]) }
  //println(a)

 // val traversed = list.traverse(identity)

  //println(traversed)



}
