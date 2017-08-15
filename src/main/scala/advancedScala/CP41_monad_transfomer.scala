package advancedScala

import advancedScala.CP41_monad_transfomer.ErrorOrOption
import cats.data.Writer

import scala.concurrent.{Await, Future}

/**
  * Created by kentaro.maeda on 2017/08/15.
  */
object CP41_monad_transfomer extends App{

  // 動機、異なる種類のモナドが入れ子になることがある。
  // Error: DBの接続不正(JavaだとRuntimeExceptionになりそうな)
  // Option: Some 結果があった。 None: 結果がなかった。
  case class User(name:String)
  def lookupDb(id:String):Either[Error, Option[User]] = Right(Option(User("a")))
  // 呼び出し側は、2つのモナドに包まれた値を操作するために、 for をネストしないといけない。
  val user:Either[Error, Option[String]] = for (result <- lookupDb("hoge"))yield {
    for (user <- result) yield  user.name
  }

  // コレを解決するために、いくつかのモナドでは合成可能なMonad transformer を提供している。
  // EitherT, OptionT などがある
  // List[Option[T]] を解決する。
  import cats.data.OptionT

  /**
    * Note how we build it from the inside out: we pass List, the type of the outer monad, as a parameter to OptionT, the transformer
    for the inner monad.
    扱うのは、 List[Option]だが、 トランスフォーマの型エイリアスは、 内部モナド[外側モナド] の順になることに注意。
    */
  type ListOption[A] = OptionT[List, A]

  import cats.Monad
  import cats.instances.list._
  import cats.syntax.applicative._
  // 生成には、 pure を使うと楽。
  val result: ListOption[Int] = 42.pure[ListOption]
  val a = 10.pure[ListOption]
  val b = OptionT.liftF(List(10, 20, 30)) // 外側モナド値を、OptionT に包むにはこれ。

  val plus = for (_a <-a; _b <- b)yield (_a+_b)
  // result: ListOption[Int] = OptionT(List(Some(42)))
  println(plus.value)

  // Cats では、XxTという形で各種トランスフォーマを提供、全て XxT[M, A] 形式で、 Mは外型のラップするモナド、Aはトランスフォーマが包む型。
  // 圏論的には、Kleisli というものらしい。
  // XxT[M,A]が、実際は M[Xx[A]] という風に、内と外が逆転することに注意。
  // building monad stacks from the inside out(モナドスタックを中から外へ構築する)

  // また、Either のような型パラメータを2つ取る形式のものは type alias で型を絞り込む必要がある。
  import cats.instances.either._
  type ErrorOr[A] = Either[Error, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]
  // これで前述の ネストモナド問題が解消できる
  def lookupDb2(id:String):ErrorOrOption[User] = User("A").pure[ErrorOrOption]
  val user2 = lookupDb2("A").map(_.name)

  println(user2.value) //value で元の値を取得できる

  // 3段ネスト
  import cats.data.EitherT
  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  import scala.concurrent.ExecutionContext.Implicits.global
  import cats.instances.future._
  import cats.syntax.option._
  import cats.syntax.either._


  val answer: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 10.pure[FutureEitherOption]
    } yield a + b
  // value のたびに、 T が一つずつ外れる。
  println(answer.value.value) // Futuer

  // XxTと Xx は同じメソッドを提供する
  // XxT はメソッドの内部でだけ使用し、外部に露出しないようにする。最後に戻り値を返すときに、Tを外すようにする。

  type Logged[A] = Writer[List[String], A]
  // Example method that returns nested monads:
  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None => Writer(List(s"Failed on $str"), None)
    }
  // Example combining multiple calls to parseNumber:
  def addNumbers(
    a: String,
    b: String,
    c: String
  ): Logged[Option[Int]] = {
    import cats.data.OptionT
    // Transform the incoming stacks to work on them:
    val result = for {
      a <- OptionT(parseNumber(a)) // parseNumberはネストモナドなので、OptionTに包むことで、yield を平坦化できる。
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c // parseNumberのままだと、ここで forのネストが必要になる。
    // Return the untransformed monad stack:
    result.value // 最後に、valueで OptionTをはがす
  }
  // This approach doesn't force OptionT on other users' code:
  val result1 = addNumbers("1", "2", "3") // 普通にWriterで取得できる。
  println(result1.run)

  // 演習
  //type Response[A] = Future[Either[String, A]] Q1,これをトランスフォーマに書き直す
  type Response[A] = EitherT[Future, String, A]
  // ボットからパワーレベルを取得する
  // def getPowerLevel(autobot: String): Response[Int] = ???
  // Q2 実装 次のデータから取得できない場合は、エラーメッセージをLeftで返す。
  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )
  def getPowerLevel(autobot: String): Response[Int] = powerLevels.get(autobot) match {
    case Some(level) => level.pure[Response]
    // ↓でいい。case _ => EitherT[Future, String, Int](Future("error".asLeft[Int]))
    case _ => EitherT.left(Future(s"$autobot unreachable"))
  }
  val ziped = getPowerLevel("aaa").value.zip(getPowerLevel(("Jazz")).value)
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  val zipres = Await.result(ziped, 3.seconds)
  println(zipres)

  // 2つのロボットのパワーレベルの合計が15なら、true
  def canSpecialMove(
    ally1: String,
    ally2: String
  ): Response[Boolean] = for (p1 <- getPowerLevel(ally1); p2 <- getPowerLevel(ally2)) yield (p1 + p2 > 15)

  // パワーレベルに応じてメッセージを取得する。
  def tacticalReport(
    ally1: String,
    ally2: String
  ): String = {
    val futureRes = canSpecialMove(ally1,ally2).value
    val res = Await.result(futureRes, 3.seconds)

    res match {
      case Right(false) => s"$ally1 and $ally2 need a recharge"
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Left(message) => s"Comes error $message"
    }
  }

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "hogegege"))
}
