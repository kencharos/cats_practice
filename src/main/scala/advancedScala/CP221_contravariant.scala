package advancedScala

/**
  * Created by kentaro.maeda on 2017/08/13.
  */
object CP221_contravariant extends App{

  // Contramap
  // f:B=>A という map。 contramp(B=>A) を定義できてしまえば、 Aがある型のInstansなら、Bもその型のInstanseにできる性質。
  // テキストでは、Printable[A] に対して、 Box[A] 型に、 BOX[A] => B へ contrampを定義することで、StringやIntがPrintableのインスタンスなら、
  // Box[String], Box[Int]も Printablのインスタンスにしていた。共変みたいなもの。

  // Printableに contramapを追加してみる

  trait Printable[A] {
    def format(value: A): String
    def contramap[B](func: B => A): Printable[B] = {
      val self = this // self is Printable[A]
      new Printable[B] {
        def format(value: B): String =
        self.format(func(value))
      }
    }
  }

  object Printable {
    def apply[A](implicit a:Printable[A]) = a
  }

  case class Box[A](value:A)

  // Box[A] => A の定義 defなのは、[A]がいるから。
  def boxPrintable[A](implicit p: Printable[A]):Printable[Box[A]] =
    // p.contramap[Box[A]](_.value) これではわかりづらいので、下。
    p.contramap((b:Box[A]) => b.value)

  //これでもいいけど、contramapがあるなら、そっちの方が楽。
  implicit def boxPrintable2[A](implicit p: Printable[A]) =
    new Printable[Box[A]] {
      def format(value: Box[A]): String =
        "Box(" + p.format(value.value) + ")"
    }


  implicit val stringPrintable =
    new Printable[String] {
      def format(value: String): String =
        "\"" + value + "\""
    }
  implicit val booleanPrintable =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  println(Printable[Boolean].format(true))
  println(Printable[String].format("hello"))
  println(Printable[Box[String]].format(Box("in the Box")))

}
