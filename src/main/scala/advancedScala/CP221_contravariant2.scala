package advancedScala


/**
  * Created by kentaro.maeda on 2017/08/13.
  */
object CP221_contravariant2 extends App{

  // Contramap
  // cats の contravariantを使う
  trait Printable[A] {
    def format(value: A): String
  }

  object Printable {
    def apply[A](implicit a:Printable[A]) = a
    def format[A](value:A)(implicit a:Printable[A]) = a.format(value)
  }

  implicit val stringPrintable:Printable[String] = new Printable[String] {
      def format(value: String): String =
        "\"" + value + "\""
    }

  def pp():Unit = {
    // functorの中にある。
    import cats.functor.Contravariant
    // Contravariantの Printable インスタンスを作っておく。
    implicit val contravariantPrintable = new Contravariant[Printable] {
      def contramap[A, B](fa: Printable[A])(f: (B) => A): Printable[B] = new Printable[B] {
       def format(value: B): String =
          fa.format(f(value))
       }
    }

    final case class Box[A](value:A)
    // Boxに対する具体的なcontrampを定義すればBoxのPrintableインスタンスができる。
    implicit def boxPrintable[A](implicit a:Printable[A]) = Contravariant[Printable].
      contramap(stringPrintable)((x:Box[A]) => a.format(x.value))

    println(Printable[String].format("hello"))
    println(boxPrintable.format(Box("in the Box")))
  }


  // show, String -> Symbol

  import cats.Show
  import cats.functor.Contravariant
  import cats.instances.string._
  val showString = Show[String]
  implicit val showSymbol = Contravariant[Show].
    contramap(showString)((sym: Symbol) => s"'${sym.name}")
  println(showSymbol.show('dave))
  println(Show[Symbol].show('hoge))

  pp()
}
