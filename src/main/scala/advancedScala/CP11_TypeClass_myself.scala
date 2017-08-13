package advancedScala

import java.util.Date

/**
  * Created by kentaro.maeda on 2017/08/08.
  */
object CP11_TypeClass_myself {

  trait Printable[A] {
    def format(a:A):String
  }

  object PrintableInstances {

    implicit val intPrintable:Printable[Int] = new Printable[Int] {
      def format(a:Int) = a+""
    }

    implicit val stringPrintable:Printable[String] = new Printable[String] {
      def format(a:String) = a
    }

    implicit val catPrintable:Printable[Cat] = new Printable[Cat] {
      override def format(a: Cat): String = s"NAME ${a.name} age:${a.age} color:${a.color}"
    }
  }

  object Printable {
    def format[A](a:A)(implicit evidence:Printable[A]):String = evidence.format(a)
    def print[A](a:A)(implicit evidence:Printable[A]):Unit = println(format(a))

    def apply[A](implicit a: Printable[A]) = a
  }

  def objectStyle():Unit = {
    import PrintableInstances._
    // Instance スタイルだと、メソッドに型クラスのインスタンスを渡す
    Printable.print("2")
    Printable.print(34)
    Printable.print(Cat("Taka", 34, "blue"))

    // apply経由にすることで、インスタンスを取得するパターンのほうが、catsでは使われるっぽい
    Printable[String].format("222")

  }

  final case class Cat(
    name: String,
    age: Int,
    color: String
  )

  // sytax style Execise3
  object PrintableSyntax {
    implicit class PrintOps[A](value:A) {
       def format(implicit a: Printable[A]): String = {
         a.format(value)
       }

      def print(implicit a: Printable[A]): Unit = {
          println(format)
      }
    }
  }


  def syntaxStyle():Unit = {
    import PrintableSyntax._
    import PrintableInstances._
    // Syntax 型だと、インスタンスメソッドが増えたように見える。
    // Kotlinの拡張メソッドみたい。
    val cat = Cat("C", 34, "red")
    cat.print
  }

  def main(args:Array[String]):Unit = {
    objectStyle()
    syntaxStyle()
  }

}
