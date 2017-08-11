package advancedScala

/**
  * Created by kentaro.maeda on 2017/08/11.
  */
object CP21_Monoid extends App{

  import cats.Monoid
  import cats.syntax.monoid._ // combineとか、 |+| とか
  import cats.syntax.option._  // .someとか
  import cats.instances.option._ // Optionに対する Monoid 型クラスのインスタンスの提供
  import cats.instances.int._ // Option[A]のMonoidを使う場合、 A型も Monoidインスタンスである必要がある。ここでは Int


  val one = 1.some
  val two = Some(2)
  val none = None
  val sum = one.combine(two)
  val sum2 = one |+| none |+| None

  println(sum)
  println(sum2)

  // cats.kernel は最低限の実装を提供するサブプロジェクト

  // モノイドを使うと、tupleの加算や Mapの加算もできる。

  import cats.instances.string._
  import cats.instances.map._
  import cats.instances.tuple._

  val pair = (1, "A")
  println (pair |+| pair) // (2, AA)

  val map1 = Map("A" -> 1, "B" -> 2)
  val map2 = Map("A" -> 2, "C" -> 3)

  println(map1 |+| map2)  // Map(A -> 3, C -> 3, B -> 2)

}
