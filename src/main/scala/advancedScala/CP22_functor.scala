package advancedScala

/**
  * Created by kentaro.maeda on 2017/08/12.
  */

import cats.syntax.functor._

import cats.Functor
import cats.instances.function._ // ListやOptionは mapがすでにあるので、function型で、
import cats.syntax.functor._
import cats.instances.option._
import cats.syntax.option._

object CP22_functor extends App{
  // Functorで、 HigherKind  F[_] 型が登場する。コレをインポートしないと警告がでるらしい。
  import scala.language.higherKinds

  val f = (x:Int) => x+1
  // できない。。 f.map(f)

  val s = 1.some

  // liftはなぜか syntaxがない。
  val lifted = Functor[Option].lift(f)

  println(lifted(s))

  println(s as 890) // as などいくつかのメソッドがある。

  // Eecise
  // 自作型への functor
  sealed trait Tree[+A] // 変異させとく
  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  //ここの型パラメータに、 Tree[A] とかしなくてよい。Kindが絡む
  implicit val treeFunctorInstance:Functor[Tree] = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(v) => Leaf(f(v))
    }
  }
  // スマートコンストラクタ。Branchや、Leafであっても Tree型として返すようにしている。
  // こうしないと、次のように syntax styleで mapが呼べない。
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)
  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  val test = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  val test2 = branch(branch(leaf(1), leaf(2)), leaf(3))
  val test3:Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)) //A の型まで明示しないといけないが、これでもよい。
  println (Functor[Tree].map(test)(x => x + 1)) // case class の Applyで作った testは、Branch型なのでimplicitの解決ができない。
  println (test2.map(x => x + ""))
  println (test3.map(x => x + 34))

}
