package advancedScala.usecase

object BuildValidator2 extends App{

  // 単一のチェックとしてPredicate を導入する・
  import cats.Semigroup
  import cats.data.Validated
  import cats.syntax.semigroup._ // |+| syntax
  import cats.syntax.applicative._ // |@| syntax
  import cats.data.Validated._ // Valid and Invalid
  import cats.syntax.apply._
  import cats.syntax.validated._

  object Predicate {
    final case class And[E, A](
                                left: Predicate[E, A],
                                right: Predicate[E, A]) extends Predicate[E, A]
    final case class Or[E, A](
                               left: Predicate[E, A],
                               right: Predicate[E, A]) extends Predicate[E, A]
    final case class Pure[E, A](
                                 func: A => Validated[E, A]) extends Predicate[E, A]
    def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] =
      Pure(f)
    def lift[E, A](error: E, func: A => Boolean): Predicate[E, A] =
      Pure(a => if(func(a)) a.valid else error.invalid)
  }

  sealed trait Predicate[E, A] {
    import Predicate._
    def and(that: Predicate[E, A]): Predicate[E, A] =
      And(this, that)
    def or(that: Predicate[E, A]): Predicate[E, A] =
      Or(this, that)
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)
        case And(left, right) =>
          // 動かないので書き換え
          (left(a).product(right(a))).map(_ => a)
        case Or(left, right) =>
          left(a) match {
            case Valid(a1) => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a2) => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  // Predicate をまとめるものとして checkを導入する。
  // check 変換用の map, flatmapを備える。
  // map も ADTで。
  import cats.Semigroup
  import cats.data.Validated
  sealed trait Check[E, A, B] {
    import Check._
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]
    def map[C](f: B => C): Check[E, A, C] =
      Map[E, A, B, C](this, f)
    // flatMapのために、eitherに戻す
    def flatMap[C](f: B => Check[E, A, C]) =
      FlatMap[E, A, B, C](this, f)
    // 複数チェックの連結
    def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
      AndThen[E, A, B, C](this, that)
  }
  object Check {
    // チェック後の結果を別の型に変換する。
    final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
      def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check(in).map(func)
    }
    final case class Pure[E, A, B](func: A => Validated[E, B]) extends Check[E, A, B] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] =
        func(a)
    }

    // flatMapをValidate葉供えていないので、一旦 eitherにする。
    // flatMapを使うと、前のチェック結果に応じた（文脈を加味した）チェック連携ができる
    final case class FlatMap[E, A, B, C](
                                          check: Check[E, A, B],
                                          func: B => Check[E, A, C]) extends Check[E, A, C] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check(a).withEither(_.flatMap(b => func(b)(a).toEither))
    }
    // flatMapによる連携
    final case class AndThen[E, A, B, C](
                                          check1: Check[E, A, B],
                                          check2: Check[E, B, C]) extends Check[E, A, C] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check1(a).withEither(_.flatMap(b => check2(b).toEither))
    }

    final case class PurePredicate[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
        pred(a)
    }

    //Predicateからつくるやつ
    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = PurePredicate(pred)
    // Predicateを関数から一気に作るやつ
    def apply[E, A, B](func: A => Validated[E, B]): Check[E, A, B] = Pure(func)
  }


  // try
  import cats.data.{NonEmptyList, OneAnd, Validated}
  import cats.instances.list._
  import cats.syntax.cartesian._
  import cats.syntax.validated._

  // 基本型。ここを変えることでいろいろなエラーメッセージを取得できる。
  type Errors = NonEmptyList[String]

  // 基本 Predicate.単一チェックと、メッセージ
  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)
  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n)
  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))
  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char))
  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)


  // 業務ルールにも度付くチェック。
  // A username must contain at least four characters
  // and consist entirely of alphanumeric characters
  val checkUsername: Check[Errors, String, String] = Check(longerThan(3) and alphanumeric)


  //And here’s the implementa􀦞on of checkEmail, built up from a numberof smaller components:
  // An email address must contain a single `@` sign.
  // Split the string at the `@`.
  // The string to the left must not be empty.
  // The string to the right must be
  // at least three characters long and contain a dot.

  // チェックとメッセージのその場生成もできるようにした
  // @で文字を分割し、それぞれでチェックができるようにする。
  val splitEmail: Check[Errors, String, (String, String)] = Check(s =>s.split('@') match {
    case Array(name, domain) => (name, domain).validNel[String]
    case other => "Must contain a single @ character".invalidNel[(String, String)]
  })
  val checkLeft: Check[Errors, String, String] =
  Check(longerThan(0))
  val checkRight: Check[Errors, String, String] =
    Check(longerThan(3) and contains('.'))

  // emailについてチェックが終わったら、@で再び結合する。
  val joinEmail: Check[Errors, (String, String), String] =
    Check { case (l, r) =>
      (checkLeft(l).product(checkRight(r))).map{case (p,t)=> p+"@"+t }}
  val checkEmail: Check[Errors, String, String] =
    splitEmail andThen joinEmail

  final case class User(username: String, email: String)
  // 最後の組み立て。
  def createUser(username: String,
                email: String): Validated[Errors, User] =
    (checkUsername(username).product(checkEmail(email))).map{case (n, s) => User(n,s)}

  println(createUser("Noel", "noel@underscore.io"))

}
