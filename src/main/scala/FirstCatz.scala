/**
  * Created by kentaro.maeda on 2017/08/07.
  *
  * Catz を始めるにあたり、型クラス in Scala の基本。
  */
object FirstCatz extends App{
  // 普通の fold の定義。
  def sumInts(list: List[Int]): Int = list.foldRight(0)(_ + _)

  def concatStrings(list: List[String]): String = list.foldRight("")(_ ++ _)

  def unionSets[A](list: List[Set[A]]): Set[A] = list.foldRight(Set.empty[A])(_ union _)

  // trait モノイドの導入
  trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A
    def |+|(x:A, y:A) = combine(x,y)
  }

  // モノイドの int 実装
  implicit val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y
  }
  // String 実装
  implicit val StringAdditionMonoid: Monoid[String] = new Monoid[String] {
    def empty: String = ""
    def combine(x: String, y: String): String = x + y
  }

  // fold の汎用化。Monoid 実装について動くようになる。
  def combineAll[A](list: List[A])(implicit A: Monoid[A]): A = list.foldRight(A.empty)(A.combine)
  def |+|[A](x:A, y:A)(implicit A: Monoid[A]): A = A.combine(x,y)



  println(combineAll(List(1,2,3,4,4)))
  println(combineAll(List("2", "3")))

  // ただの pair
  final case class Pair[A, B](first: A, second: B)
  // Monoid pairの生成メソッド。defの引数も implicit にするのがポイント。
  implicit def deriveMonoidPair[A, B](implicit A: Monoid[A], B: Monoid[B]): Monoid[Pair[A, B]] =
    new Monoid[Pair[A, B]] {
      def empty: Pair[A, B] = Pair(A.empty, B.empty)

      def combine(x: Pair[A, B], y: Pair[A, B]): Pair[A, B] =
        Pair(A.combine(x.first, y.first), B.combine(x.second, y.second))
    }

  println(combineAll(List(Pair(1,"A"), Pair(3, "B"))))
}
