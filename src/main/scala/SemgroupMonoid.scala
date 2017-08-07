import cats.Semigroup
import cats.Semigroup._
import cats.Monoid
import cats.syntax.semigroup._

object SemgroupMonoid extends App{

  // 特定の型の Semigroup実装を用意すると、
  implicit val intSemigrop: Semigroup[Int] = new Semigroup[Int] {
      def combine(x:Int, y:Int) = x + y
  }
  implicit val StringSemigrop: Semigroup[String] = new Semigroup[String] {
    def combine(x:String, y:String) = x + y
  }
  // Semigroupの各メソッドが使えるようになる。
  println(combine(3, 4))
  println(combineN("0", 9))
  println(3 |+| 4) // from  syntax.semigroup。マクロでよしなにしてくれる
  // Monoid は Semigorupに、empty を足したもの

}
