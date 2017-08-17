package advancedScala

import cats.{Applicative, Eval, Foldable, Traverse}
import cats.kernel.Monoid

/**
  * Created by kentaro.maeda on 2017/08/16.
  */
object CP71_foldable extends App{

  //Fordable, foldleft, foldRight を提供するもの

  //Traversable, F[G[A]]を G[F[A]] にA の構造を保ったまま、F,Gを入れ替える。　Foldableの一般化。

  def foldable(): Unit = {

    // 演習1 空リストと、:: の fold
    println(List(1,2,3).foldLeft(List[Int]())((acc, a) => a :: acc )) //reverse
    println(List(1,2,3).foldRight(List[Int]())(_::_)) // 同じ

    // 演習2 fold で map, filter, flatMap, sum
    def sum_(list:List[Int]) = list.foldLeft(0)(_ + _) // Monoid使えば一般化できる。
    def sum2_[A](list:List[A])(implicit m:Monoid[A]):A = list.foldLeft(m.empty)(m.combine)
    println(List(1,2,3).sum== sum_(List(1,2,3)))
    import cats.instances.string._
    println(sum2_(List("2", "3","4")))
    def map_[A,B](list:List[A])(f:A=>B):List[B] = list.foldRight(List[B]())(f(_) :: _)
    println(List(1,2,3).map(_+1) == map_(List(1,2,3))(_+1))
    def filter_[A](list:List[A])(f: A=> Boolean):List[A] = list.foldRight(List[A]()){
      (a, acc) => if(f(a)) a :: acc else acc
    }
    println(List(1,2,3).filter(_ % 2 == 0) == filter_(List(1,2,3))(_ % 2 == 0))
    def flatMap_[A,B](list:List[A])(f:A=> List[B]):List[B] = {
      list.foldRight(List[B]()){ (a, acc) => f(a) ++ acc }
    }
    println(List(1,2,3).flatMap(a => List.fill(a)(a)) == flatMap_(List(1,2,3))(a => List.fill(a)(a)))

    // flatMapで、全て定義できる。
    def map2_[A,B](list:List[A])(f:A=>B):List[B] = flatMap_(list)(x => List[B](f(x)))
    def filter2_[A](list:List[A])(f: A=> Boolean):List[A] = flatMap_(list)(x => if(f(x)) List(x) else List[A]())
    println(List(1,2,3).filter(_ % 2 == 0) == filter2_(List(1,2,3))(_ % 2 == 0))
    println(List(1,2,3).map(_+1) == map2_(List(1,2,3))(_+1))

    // cats foldable
    // list, vector など fold操作を持つものはそのまま委譲、
    // それ以外は、Folable instanse の内容による
    import cats.instances.list._
    import cats.instances.option._
    val foldableString = Foldable[List].foldLeft(List("2ssss","3sss"), "init")(_ + _)
    val foldableOpt = Foldable[Option].foldLeft(Option(123), 10)(_ * _)
    println(foldableString)
    println(foldableOpt)

    // map もいける, mapは value について、まとめる。
    import cats.instances.map._
    type StringMap[A] = Map[String, A]
    val stringMap = Map("a" -> "b", "c" -> "d")
    val mapFoldable =  Foldable[StringMap].foldRight(stringMap, Eval.now("n"))((a, ev)=> ev.map(_ + "," + a))
    println(mapFoldable.value) // foldRight はスタックセープのため、 Eval でやる。

    // 他に、monoid の |+| で sumを求める combineAll, map後の Monoid[B]で、 sum を求める foldMap がある。
    import cats.instances.int._
    println(Foldable[List].combineAll(List(1,2,3)))
    println(Foldable[List].foldMap(List(1,2,3))(_ *10 + ""))
    println(Foldable[List].foldK(List(Option(1), Option(3)))) //?

    // compose により、ネストしたリストをフラットにして、foldできる。
    import cats.instances.vector._ // Monoid of Vector
    val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
    println((Foldable[List] compose Foldable[Vector]).combineAll(ints))

    // もちろん、syntax もある
    import cats.syntax.foldable._

    println(Option(23).foldMap(_.toString + "AAA"))
    println(List(1,2,3,4).foldMap(_.toString))
    import cats.instances.option._
    val f = (s:String, i:Int) => if(i > 0) Option(s + i) else None
    println(List(1,2,3,-4).foldM("res")(f)) // 結果をモナドにすることも可能

  }

  def traversable():Unit = {
    import cats.Traverse
    import cats.instances.option._
    import cats.instances.int._
    import cats.instances.list._
    import cats.instances.future._
    import cats.syntax.traverse._
    import cats.syntax.option._
    import cats.syntax.applicative._

    // traverse のよくある例
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    val hostnames = List(
      "alpha.example.com",
      "beta.example.com",
      "gamma.demo.com"
    )
    def getUptime(hostname: String): Future[String] = Future(s"Get $hostname")

    // 1件ずつFutureで取得した List[Future] を、まとめたい
    val allGet = Traverse[List].traverse(hostnames)(getUptime)
    // traverse を一般化して、sequense
    val allGet2 = hostnames.map(getUptime).sequence // List[Future] => Future[List]

    val f = Await.result(allGet.zip(allGet2), 3.seconds)
    println(f)

    // 演習1
    import cats.instances.vector._
    import cats.syntax.apply._
    import cats.instances.option._
    import cats.syntax.cartesian._

    import scala.language.higherKinds

    // traverse は Applicative から作ること事ができる。
    def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
      list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
        (accum |@| func(item)).map(_ :+ _)
      }

    // sequence は Traverseから作れる。
    def listSequence[F[_] : Applicative, A](list: List[F[A]]):F[List[A]] = {
      listTraverse(list)(identity)
    }

    val vs = listSequence(List(Vector(1, 2), Vector(3, 4)))
    println(vs) // Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
    // Vectorはモナドなので、 flatMapベースでの計算が行われる
    println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5,6))))
    println(listSequence(List(Option(3), Option(8)))) // Some(List(3,8))

    def process(inputs: List[Int]):Option[List[Int]] =
        listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

    println(process(List(1,2,3))) // None
    println(process(List(2,4,6))) // Some(List(2, 4, 6))

    // Validated in Foldable
    import cats.data.Validated
    import cats.instances.list._ // Applicative[ErrorsOr] needs a
    type ErrorsOr[A] = Validated[List[String], A]
    def processV(inputs: List[Int]): ErrorsOr[List[Int]] = {
      def go(n:Int) :ErrorsOr[Int] = {
        (n % 2) match {
          case 0 => Validated.valid(n)
          case _ => Validated.invalid(List(s"$n is not even"))
        }
      }
      listTraverse(inputs)(go)
    }
    println(processV(List(1,2,3))) // Invalid(List(1 is not even, 3 is not even))
    println(processV(List(12,2,4))) // Valid(List(12, 2, 4))

    // 自作クラスに、Travarse できるようにする。
    trait Tree[+A]
    case class Node[A](v:A) extends Tree[A]
    case class Leaf[A](left:Tree[A], right:Tree[A]) extends Tree[A]
    def node[A](v:A):Tree[A] = Node(v)
    def leaf[A](left:Tree[A], right:Tree[A]) = Leaf(left, right)

    val target = leaf(node(3), leaf(node(1), node(3)))
    println(target)

    implicit val applicativeTree:Applicative[Tree] = new Applicative[Tree] {
      override def pure[A](x: A): Tree[A] = node(x)
      override def ap[A, B](ff: Tree[(A) => B])(fa: Tree[A]): Tree[B] = {
        (ff, fa) match {
          case (Node(f), Node(a)) => node(f(a))
          case (lift@Node(f), Leaf(a1, a2)) => leaf(ap(lift)(a1), ap(lift)(a2))
          case (Leaf(f1, f2), Leaf(a1, a2)) => leaf(ap(f1)(a1), ap(f2)(a2))
          case (Leaf(f1, f2), n@Node(a1)) => ap(f1)(n)
        }
      }
    }
    // map2 が使えるように。
    println(Applicative[Tree].map2(target, target)(_ + _))

    val target2 = leaf(node(Some(2)), leaf(node(Some(1)), node(Some(3))))

    // sequence を使うには、Foldable も必要。 つらいので、オプションでやった。
    /*
    implicit val foldableTree:Traverse[Tree] = new Traverse[Tree] {
      override def foldLeft[A, B](fa: Tree[A], b: B)(f: (B, A) => B): B = fa match {
        case Node(x) => f(b,x)
        case Leaf(x1, x2) =>
      }
      override def foldRight[A, B](fa: Tree[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
      override def traverse[G[_], A, B](fa: Tree[A])(f: (A) => G[B])(implicit evidence$1: Applicative[G]): G[Tree[B]] = ???
    }
    */
  }

  // Applicatice インスタンスを作る練習
  def practice():Unit = {

    implicit val applicativeOption:Applicative[Option] = new Applicative[Option] {
      override def pure[A](x: A): Option[A] = Option(x)
      override def ap[A, B](ff: Option[(A) => B])(fa: Option[A]): Option[B] = {
        println("自作applicative...")
        (ff, fa) match {
          case (Some(f), Some(a)) => Some(f(a))
          case _ => None
        }
      }
    }


    implicit val traverseOption:Traverse[Option] = new Traverse[Option] {
      override def traverse[G[_], A, B](fa: Option[A])(f: (A) => G[B])(implicit ap: Applicative[G]): G[Option[B]] = {
        println("自作trav")
        fa match {
          case Some(x) => ap.map(f(x))(i => Some(i))
          case None => ap.pure(None)
        }
      }
      override def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa match {
        case Some(x) => f(b, x)
        case None => b
      }
      override def foldRight[A, B](fa: Option[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
        case Some(x) => f(x, lb)
        case None => lb
      }
    }
    println(Applicative[Option].map2(Some(7), Some(8))(_ * _))
    import cats.instances.list._
    println(Traverse[Option].sequence(Option(List(1,2))))
    println(Applicative[List].sequence(Option(List(1,2)))) // OptionFoldableが使われるのはこっち

  }
  foldable()
  traversable()
  practice()

}
