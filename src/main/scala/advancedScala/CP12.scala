package advancedScala

import java.util.Date

/**
  * Created by kentaro.maeda on 2017/08/09.
  */
object CP12 extends App{

  // TypeClass は cats パッケージ
  import cats.Show

  case class Sample(name:String)
  implicit val showSample:Show[Sample] = new Show[Sample] {
    override def show(t: Sample): String = t.name
  }

  // TypeClassのapply[型]で、当該型のShowインスタンスを得ることができる
  // ただし、implictパラメータがある場合のみ (applyは、implicit Show[T] を受け取るようになっているので。
  val showSample_ = Show.apply[Sample]
  //宣言していない型の applyは失敗する
  //val showInt = Show.apply[Int] // Intの Show インスタンスはないためエラー。

  // Intなど基本的な型クラスのインスタンスは既に用意さている。cats.instances.パッケージ
  import  cats.instances.int._
  import cats.instances.string._

  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]

  println(showInt.show(45))
  println(showString.show("aaa"))

  // ↑はあんまりなので、syntax で拡張メソッド風に使えるようになる。 cats.syntax パッケージ
  import cats.syntax.show._
  println(235.show)
  println("Hello".show)
  println(Sample("Ken").show)

  // showインスタンスを手軽に作るメソッドも用意されている。
  implicit val dateShow:Show[Date] = Show.show(date => s"${date.getTime}ms since the epoch.")

  println(new Date().show)
}
