package advancedScala.usecase

object BuildValidator3 extends App{


  import cats.Semigroup
  import cats.data.Validated
  import cats.data.Validated._
  import cats.syntax.semigroup._
  import cats.syntax.validated._

  import cats.data.{Kleisli, NonEmptyList, Validated}
  import cats.instances.either._
  import cats.instances.function._
  import cats.instances.list._
  import cats.syntax.cartesian._
  import cats.syntax.validated._


  // クライスリを使うバージョン
  // クライスリは次のような andThenを備えた構造
  val step1: Kleisli[List, Int, Int] = Kleisli(x => List(x + 1, x - 1))
  val step2: Kleisli[List, Int, Int] = Kleisli(x => List(x, -x))
  val step3: Kleisli[List, Int, Int] =Kleisli(x => List(x * 2, x / 2))
  println( (step1 andThen step2 andThen step3).run(8) ) //flatMapで連結したものを、runで流す。
  // よって、クライスリは今までやってきた Check を一般化したものとなる。
  // よくわからんが、flatMapの機能を切り出して一般化したものっぽい。
  // また、クライスリはモナドなので、Validatedよりも、Either[List]の方が良いようだ。

  type Errors = NonEmptyList[String]
  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)
  type Result[A] = Either[Errors, A]
  // Krisriを使うと、Check実装が無くなる。
  type Check[A, B] = Kleisli[Result, A, B]
  def check[A, B](func: A => Result[B]): Check[A, B] = Kleisli(func)
  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
    Kleisli[Result, A, A](pred.run)

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
    def run(implicit s: Semigroup[E]): A => Either[E, A] =
      (a: A) => this(a).toEither

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



  // try
  import cats.data.{NonEmptyList, Validated}
  import cats.syntax.validated._

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

  val checkUsername: Check[String, String] =
    checkPred(longerThan(3) and alphanumeric)

  val splitEmail: Check[String, (String, String)] =
    check(_.split('@') match {
      case Array(name, domain) =>
        Right((name, domain))
      case other =>
        Left(error("Must contain a single @ character"))
    })
  val checkLeft: Check[String, String] =
    checkPred(longerThan(0))
  val checkRight: Check[String, String] =
    checkPred(longerThan(3) and contains('.'))
  val joinEmail: Check[(String, String), String] =
    check { case (l, r) =>
      (checkLeft(l) |@| checkRight(r)).map(_+"@"+_) }
  val checkEmail: Check[String, String] =
    splitEmail andThen joinEmail

  final case class User(username: String, email: String)
  def createUser(username: String, email: String): Either[Errors,
    User] = (
    checkUsername.run(username) |@|
      checkEmail.run(email)
    ).map(User)

  println(createUser("Noel", "noel@underscore.io"))

}
