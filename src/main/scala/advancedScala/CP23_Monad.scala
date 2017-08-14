package advancedScala

/**
  * Created by kentaro.maeda on 2017/08/14.
  */
object CP23_Monad extends App {
  import scala.language.higherKinds
  import cats.Monad
  import cats.instances.option._
  import cats.instances.list._
  import cats.syntax.monad._
  import cats.syntax.functor._
  import cats.syntax.flatMap._

  // Monad を実装しているもの全般についての演算が定義できる
  def plus[M[_]:Monad](a:M[Int], b:M[Int]):M[Int] = {
    for(_a <- a; _b <- b)yield _a+_b
  }

  println(plus(Option(1), Option(2)))

  println(plus(List(1,2), List(3,4)))

  // M値を必要としている関数に、何もしない Monad, Id をかませることができる
  import cats.Id
  // Idの実体は type alias なので、型宣言を付与するだけでOK
  // あるいは、pure を使うこともできる
  // monad の関数も使える
  println(plus(3:Id[Int], Monad[Id].pure(4).map{_ + 1} >>= (_ + 3:Id[Int])))


}
