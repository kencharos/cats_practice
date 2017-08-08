package advancedScala

/**
  * Created by kentaro.maeda on 2017/08/08.
  */
object CP1 {

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
  }

  def objectStyle():Unit = {
    import PrintableInstances._
    Printable.print("2")
    Printable.print(34)
    Printable.print(Cat("Taka", 34, "blue"))
  }

  final case class Cat(
    name: String,
    age: Int,
    color: String
  )

  def main(args:Array[String]):Unit = {
    objectStyle()
  }

}
