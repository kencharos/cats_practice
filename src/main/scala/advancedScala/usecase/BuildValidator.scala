package advancedScala.usecase

import cats.Cartesian
import cats.data.ValidatedInstances

object BuildValidator{

  def basic1():Unit = {
    import cats.Semigroup
    import cats.instances.list._
    import cats.syntax.monoid._

    // エラーメッセージを足していくだけなら、semigroupでいい。Monoidでなくてもいい。
    val semigroup = Semigroup[List[String]]
    // Combination using methods on Semigroup
    println(semigroup.combine(List("Badness"), List("More badness")))
    // res2: List[String] = List(Badness, More badness)
    // Combination using Semigroup syntax
    println(List("Oh noes") |+| List("Fail happened"))
    // res4: List[String] = List(Oh noes, Fail happened)
  }

  def basic2():Unit = {
    import cats.Semigroup
    import cats.syntax.either._ // asLeft and asRight syntax
    import cats.syntax.semigroup._ // |+| syntax

    // バリデーションの結合。

    final case class CheckF[E, A](func: A => Either[E, A]) {
      // alias
      def apply(a: A): Either[E, A] =
        func(a)

      def and(that: CheckF[E, A])
             (implicit s: Semigroup[E]): CheckF[E, A] =
        CheckF { a =>
          (this (a), that(a)) match {
            case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft // どちらもエラーなら結合
            case (Left(e), Right(a)) => e.asLeft // 片方エラーならエラーを、
            case (Right(a), Left(e)) => e.asLeft
            case (Right(a1), Right(a2)) => a.asRight // 両方OKなら継続
          }
        }
    }
    // test
    import cats.instances.list._ // Semigroup for List
    val a: CheckF[List[String], Int] =
      CheckF { v =>
        if(v > 2) v.asRight
        else List("Must be > 2").asLeft
      }
    // a: CheckF[List[String],Int] = CheckF(<function1>)
    val b: CheckF[List[String], Int] =
      CheckF { v =>
        if(v < -2) v.asRight
        else List("Must be < -2").asLeft
      }
    // b: CheckF[List[String],Int] = CheckF(<function1>)
    val check = a and b
    // check: CheckF[List[String],Int] = CheckF(<function1>)
    println(check.apply(4))
    println(check.apply(0))
    println(check.apply(5))
    println(check.apply(-3))

    // エラーが無いようなケースでNothing を使うと、、
    val b2: CheckF[Nothing, Int] = CheckF(v => v.asRight)
    // Nothing がセミグループでないため、andできない。
    // b2 and b2 //error
  }

  def validated():Unit = {
    import cats.Semigroup
    import cats.data.Validated
    import cats.data.ValidatedInstances
    import cats.syntax.semigroup._ // |+| syntax
    import cats.syntax.applicative._ // |@| syntax
    import cats.data.Validated._ // Valid and Invalid
    import cats.syntax.apply._

    sealed trait Check[E, A] {
      def and(that: Check[E, A]): Check[E, A] =
        And(this, that)
      def or(that: Check[E, A]): Check[E, A] =
        Or(this, that)
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
        this match {
          case Pure(func) =>
            func(a)
          case And(left, right) => {
            // 動かん
           // (left(a) |@| right(a)).map((_,_) => a)
            left(a)
          }
          case Or(left, right) =>
            left(a) match {
              case Valid(a) => Valid(a)
              case Invalid(e1) =>
                right(a) match {
                  case Valid(a) => Valid(a)
                  case Invalid(e2) => Invalid(e1 |+| e2) // エラーメッセージの累積はモノイドの性質で行う。
                }
            }
        }
    }
    final case class And[E, A](
                                left: Check[E, A],
                                right: Check[E, A]) extends Check[E, A]
    final case class Or[E, A](
                               left: Check[E, A],
                               right: Check[E, A]) extends Check[E, A]
    final case class Pure[E, A](
                                 func: A => Validated[E, A]) extends Check[E, A]

  }

  def main(args:Array[String]):Unit = {
    println("basic1")
    basic1()
    println("basic2")
    basic2()
    println("basiec2-2")
    // 実行できない? BasicADT.basic2233()
  }
}

object BasicADT {

  def basic2233():Unit = {
    // ADTによる実装を考える
    // ADT による実装だと、構造と処理(Ingerpreter)を分けることができる。Free ?

    import cats.Semigroup
    import cats.syntax.either._ // asLeft and asRight syntax
    import cats.syntax.semigroup._ // |+| syntax
    import cats.instances.list._

    sealed trait Check[E, A] {
      def and(that: Check[E, A]): Check[E, A] =
        And(this, that)
      def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] =
        this match {
          case Pure(func) =>
            func(a)
          case And(left, right) => // ADTの判定とチェックを appliyで。Freeっぽい?
            (left(a), right(a)) match {
              case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
              case (Left(e), Right(a)) => e.asLeft
              case (Right(a), Left(e)) => e.asLeft
              case (Right(a1), Right(a2)) => a.asRight
            }
        }
    }

    final case class Pure[E, A](func: A => Either[E, A]) extends Check[E, A]
    final case class And[E, A](
                                left: Check[E, A],
                                right: Check[E, A]) extends Check[E, A]

    val a: Check[List[String], Int] =
      Pure { v =>
        if(v > 2) v.asRight
        else List("Must be > 2").asLeft
      }
    // a: wrapper.Check[List[String],Int] = Pure(<function1>)
    val b: Check[List[String], Int] =
      Pure { v =>
        if(v < -2) v.asRight
        else List("Must be < -2").asLeft
      }
    // b: wrapper.Check[List[String],Int] = Pure(<function1>)
    val check = a and b
    // check: wrapper.Check[List[String],Int] = And(Pure(<function1>),Pure(<function1>))

    println(check(8))
    println(check(3))
    println(check(4))
    println(check(-1))
  }
}
