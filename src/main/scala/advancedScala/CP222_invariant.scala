package advancedScala

/**
  * Created by kentaro.maeda on 2017/08/13.
  */
object CP222_invariant extends App{

  def pp():Unit = {

    //Invariant は Contravariantの当該型インスタンすへの昇格のほか、元に戻す性質を加える。
    // imapというメソッドを定義する。
    import cats.functor.Invariant


    // 文字列 -> 数値 にして数値型インスタンスでの計算 -> 文字列に戻す
    import cats.Semigroup
    import cats.functor.Invariant
    import cats.instances.semigroup._ // imap extension method
    import cats.instances.int._
    import cats.syntax.invariant._


    implicit val stringToIntSemigroup: Semigroup[String] =
      Semigroup[Int].imap((i: Int) => i + "")((s: String) => s.toInt)

    import cats.syntax.semigroup._
    val sum = 22 |+| 33
    val res:String = "123" |+| "222" |+| "333"
    println(sum)
    println(res)
    println(res.getClass)
  }
  pp()
}
