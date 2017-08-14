package advancedScala

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}

/**
  * Created by kentaro.maeda on 2017/08/14.
  */
object CP24_Monads_Instances extends App{

  /**
    * val, lazy, def などの評価に対するモナド。
    * メモ化や、 deffer によるスタック消費の回避などがあり、 モナドの再帰呼び出しでスタック漏れを回避する基本的なモナドとなっている。
    * state の戻り値も eval
    */
  def eval():Unit = {
    import cats.Eval
    // val
    val now = Eval.now({println("now");1 + 2;})
    // lazy
    val later = Eval.later({println("later");3 + 4;})
    // def
    val always = Eval.always({println("always");5 + 6;})

    println("call each eval")
    println(now.value)
    println(later.value)
    println(always.value)

    // 合成可能なのが面白い。合成でもそれぞれの特性を引き継ぐのも面白い
    println("composite")
    val composite = for(a <- now; b<-later; c<-always)yield (a+b+c)

    println(composite.value)
    println(composite.value)

    // deffer を使うと巨大な再帰でも、末尾再帰でなくてもスタック漏れが起きない
    // @tailrecではない
    def factorial(n: BigInt): Eval[BigInt] =
      if(n == 1) {
        Eval.now(n)
      } else {
        Eval.defer(factorial(n - 1).map(_ * n))
      }

    println(factorial(10000).value)
  }

  /**
    * Writerモナド。結果とは別に、追記可能なデータを一緒に持つ
    */
  def writer():Unit = {
    import cats.data.Writer
    import cats.instances.vector._

    val w1= Writer(Vector(
      "It was the best of times",
      "It was the worst of times"
    ), 123)
    import cats.syntax.applicative._ // `pure` method
    // Writerの方をエイリアスにすると楽。
    type Logged[A] = Writer[Vector[String], A]
    val w2 = 123.pure[Logged]
    // res2: Logged[Int] = WriterT((Vector(),123))

    println(w2.run) // 結果は、logとresultの tupple。 writen, value で個別にとれる。

    // logの追記は tell を使う (実際には tell で新しい Writerインスタンスができるかんじ)
    import cats.syntax.writer._

    val writer1 = for {
      a <- 10.pure[Logged] // log は空。
      _ <- Vector("a", "b", "c").tell // a,b,c を追記
      b <- 32.writer(Vector("x", "y", "z")) // x.y.xを追記
    } yield a + b // 最終結果を 10+32 にする。
    // logに対する mapWrittenもある
    println(writer1.mapWritten(_.map(_.toUpperCase())).map(_+1).run)

    // 課題。ステップごとにログを出す。
    // Writerの使いどころは並列処理でのスレッドごとにログを取ることが一つ
    def factorial(n: Int): Logged[Int] =
    for {
      ans <- if(n == 0) {
        1.pure[Logged]
      } else {
        factorial(n - 1).map(_ * n)
      }
      _ <- Vector(s"fact $n $ans").tell // ログ追記。ここむずいな
    } yield ans

    // 並列処理
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    println(factorial(10).run)
    val Vector((logA, ansA), (logB, ansB)) =
      Await.result(Future.sequence(Vector(
        Future(factorial(5).run),
        Future(factorial(5).run)
      )), 5.seconds)
    println(logA)
    println(logB)
  }

  /**
    * Readerモナド。
    * 読み込み専用のオブジェクトを渡して、計算を実行する処理機を作る。
    */
  def reader():Unit = {
    import cats.data.Reader
    import cats.instances.string._

    case class Db(
       usernames: Map[Int, String],
       passwords: Map[String, String]
     )
    type DbReader[A] = Reader[Db, A]

    // Reader は、apply で、取得元オブジェクト => 取得値の関数を設定して定義する。Map => String が良くある形だろう。
    // 取得元オブジェクトは、 Reader#run の引数になる。
    // 実際のオブジェクトをrunのときに初めて渡すので、モックや簡易的なDI とみなすことができる。

    // ユーザー確認
    def findUsername(userId: Int): DbReader[Option[String]] =
      Reader(db => db.usernames.get(userId))
    // パスワード確認
    def checkPassword(username: String, password: String): DbReader[Boolean] =
      Reader(db => db.passwords.get(username).contains(password))

    import cats.syntax.applicative._ // `pure` method

    // Reader は合成できる。
    def checkLogin(userId: Int, password: String): DbReader[Boolean] =
      for {
        username <- findUsername(userId) // usrnameは Option[String]
        passwordOk <- username.map { username =>
          checkPassword(username, password)
        }.getOrElse(false.pure[DbReader])// ここで Option[DBReader]から、DbReader[Option]に変えている。
      } yield passwordOk

    val mockdb = Db(Map(1 -> "test"), Map("test" -> "pass"))

    println(checkLogin(1, "pass").run(mockdb))
    println(checkLogin(1, "not").run(mockdb))
    println(checkLogin(2, "pass").run(mockdb))
  }

  /**
    * Stateモナド。mutableな状態を作るモナド
    */
  def state():Unit = {
    import cats.data.State
    // Stateは S=>(S,A) 型。 Sはステート、Aは結果。
    // Sは run時に外から渡し、flatmapごとに移り変わる想定。Aはrun内の計算結果を示す。
    val a = State[Int, String] { state =>
      (state, s"The state is $state")
    }
    println(a.run(10).value) // 結果は evalなので、valueで取る。

    import State._
    val program: State[Int, (Int, Int, Int)] = for {
      a <- get[Int] // 現在のSを取る
      _ <- set[Int](a + 1) // Sに a+1, AはUnit,
      b <- get[Int] // b = a+1, AはUnitのまま
      _ <- modify[Int](_ + 1) // S を b+1で更新
      c <- inspect[Int, Int](_ * 1000) // Sの値に関数を適用して、A を設定
    } yield (a, b, c) // 最終的な A
    // program: cats.data.State[Int,(Int, Int, Int)] = cats.data.
    val (state, result) = program.run(1).value
    println(s"${state}, ${result}")

    // 加算器を作る

    import cats.data.State

    import cats.syntax.applicative._

    // State alias
    type CalcState[A] = State[List[Int], A]
    //ヘルパ。数値をStateに包む
    def operand(num: Int): CalcState[Int] =
      State[List[Int], Int] { stack =>
        (num :: stack, num) // ステート Sの先頭に に num を追加する。
      }
    // ヘルパ。計算過程S, 計算結果A のステートを作成する
    def operator(func: (Int, Int) => Int): CalcState[Int] =
      State[List[Int], Int] {
        case a :: b :: tail => // 蓄積された S から先頭2つを取得し、計算を行ってステート先頭を結果で置き換える。
          val ans = func(a, b)
          (ans :: tail, ans)
        case _ =>
          sys.error("Fail!")
      }
    // 一文字実行
    def evalOne(sym: String): CalcState[Int] =
      sym match {
        case "+" => operator(_ + _)
        case "-" => operator(_ - _)
        case "*" => operator(_ * _)
        case "/" => operator(_ / _)
        case num => operand(num.toInt)
      }

    println(evalOne("10").run(Nil).value)
    // これはNG.println(evalOne("+").run(Nil).value)
    // 計算するためには、for合成
    val res = for(_ <- evalOne("10"); _ <- evalOne("10"); ans <- evalOne("-"))yield ans
    println(res.run(Nil).value)

    // 複数文字実行
    // ※ 可変の計算実行をまとめて実行するためには、foldを 初期値 pureで lift して、flatMapで合成するパターンを良く使うので覚えておく
    def evalAll(input: List[String]): CalcState[Int] = {
      input.foldLeft(0.pure[CalcState]) { (a, b) =>
        a flatMap (_ => evalOne(b))
      }
    }
    // 30 * 20 - 10 になる。
    println(evalAll(List("10", "20", "30", "*", "-")).run(Nil).value)

  }

  eval()
  writer()
  reader()
  state()
}
