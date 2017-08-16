package advancedScala

import scala.concurrent.{Await, Future}

/**
  * Created by kentaro.maeda on 2017/08/16.
  */
object CP61_Cartesians extends App{

  def pp():Unit= {
    // Cartesians
    // 複数値のバリデーションをファーストエラーで終わらせず全部チェックする、
    // Futuer の並列計算 (Futuerを flatmapすると、直列化してしまう)する、
    // など 2つの context(モナド) を任意の関数で結合する。
    // zip を型クラス化したものであり、 product(F[A], F[B]):F[(A,B)] を定義する
    // カルテジアン積とは、直積やデカルト積のこと。

    // Applicative functor
    // Cartesian と functor を継承し、context内の値に関数を適用する方法を提供する
    // cats の Applicative は Haskel, scalaz とちょっと違うらしい

    import cats.Cartesian
    import cats.instances.option._ // Cartesian for Option
    val a = Cartesian[Option].product(Some(123), Some("abc"))
    println(a) // Some((123,abc)) , (Some,Some)ではなく、中の値がタプルになって、もう一度 Someになるの面白い。
    // なので、これは Noneになる。
    println(Cartesian[Option].product(None, Some("abc")))

    // 2つ以上の値の積は、tupleN で作成でき、 mapN で結合方法を選べる。 Nは 22まである。
    println(Cartesian.tuple3(Option(1), Option(2), Option(3))) // Some((1,2,3)
    println(Cartesian.map3(Option(1), Option(2), Option(3))(_ + _ + _)) // Some(6)

    import cats.instances.option._
    import cats.syntax.cartesian._


    // Cartesian Biuilder を、 |@| で使用可能になる。
    // 1.0.0から Depcated になってる..
    val builder = Option(1) |@| Option(2) |@| Option(3)
    println(builder.tupled)
    println(builder.map(_ + _ + _))
    // Depcreated の変わりに、 apply の mapN を使う。
    import cats.syntax.apply._
    println((Option(1), Option(2), Option(3)).mapN(_ + _ + _))
    // tupled は、そもそもタプルでｴｴやんって感じか。 tupperにmapできるのは面白い。

    // ケースクラスにマッピングできる
    case class Cat(name: String, born: Int, color: String)

    val someCat = (Option("Garfield") |@| Option(1978) |@| Option("Orange and black")).map(Cat.apply)
    // これもこうなる
    val someCat2 = (Option("Garfield"), Option(1978), Option("Orange and black")).mapN(Cat.apply)

    // invariant .

    import cats.instances.string._
    import cats.instances.int._
    import cats.Monoid
    import cats.instances.monoid._ // わかりづらいが、 imapNの implicitparam が Invariant[Monoid]を所望している

    // これで、Cat => Tupple => Tuppleで |+| => Tuppleを Cat という流れができる。
    implicit val catMonoid = (Monoid[String], Monoid[Int], Monoid[String])
      .imapN(Cat.apply)((c: Cat) => (c.name, c.born, c.color))

    import cats.syntax.monoid._

    println(Cat("a", 12, "red") |+| Cat("b", 34, "blue"))

    // Monadかつ、Cartesian のいくつかは、product の結果が普通と異なる。
    import cats.instances.list._
    import cats.instances.either._
    println(Cartesian[List].product(List(1, 2), List(3, 4))) // flatMapと同じ
    type ErrorOr[A] = Either[Vector[String], A]
    println(Cartesian[ErrorOr].product(
      Left(Vector("Error 1")),
      Left(Vector("Error 2"))
    )) //flatMapと同じで、ファーストヒットで終わる。

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    import cats.instances.future._
    // Futureの並列実行はOK. これは、flatMapの外で、 Futuer生成しているから。
    val f = (Future({Thread.sleep(100);println("Para1");3;}), Future({println("Para2");4}), Future(5)).mapN(_ + _ + _)

    val fres = Await.result(f, 1.seconds)
    println(fres)
  }

  // Validateを使うと、エラー累積ができる
  def validate():Unit = {
    println("----validate---------------")
    import cats.Cartesian
    import cats.data.Validated
    import cats.instances.list._ // Semigroup for List
    type AllErrorsOr[A] = Validated[List[String], A]
    val vres1 = Cartesian[AllErrorsOr].product(
      Validated.invalid(List("Error 1")),
      Validated.invalid(List("Error 2"))
    )
    println(vres1)

    import cats.syntax.validated._
    import cats.syntax.monoid._
    import cats.syntax.cartesian._
    import cats.instances.string._
    import cats.instances.int._
    import cats.instances.vector._
    import cats.syntax.apply._


    // Valid, Invalid の2つの状態を持ち、1つでも Invalidがあれば Invalidになる。
    // スマートコンストラクタのほか、Either,例外などからも作成可能。
    val v1 = 1123.valid[List[String]]
    val v2 = List("NG").invalid[Int]
    val v3 = List("NG2").invalid[Int]
    println(v1 +":"+ v2)

    // 合成
    println(v1 |+| v2 |+| v3) // Semigroupであれば合成できる。ただしvalid値が同じ型に限る。
    println(v1 |+| v1 |+| v1) // Valid.

    // |@| による連結もできるが、 |+| でやったほうがいいように見える. |@| 使うのに type aliasいるし。
    // ただし、 |+| の場合、valid値も 結合されてしまうので、型が違う validatedを結合する場合は無理っぽい。
    val v4:AllErrorsOr[Int] = 1123.valid[List[String]]
    val v5:AllErrorsOr[Int] = List("NG").invalid[Int]
    val v6:AllErrorsOr[Int] = List("NG2").invalid[Int]
    println((v4 |@| v5 |@| v6).tupled)

    val v = v1 |+| v2 |+| v3
    // メソッド
    println(v.map(_ + 1)) // validへのマップ
    println(v.leftMap(_.size)) // invalidへのマップ。エラー件数になる。
    println(v.bimap(_.size, _ +1)) // left, right 同時マップ
    println(v.toEither) // Eitherへ
    // validateは monadでないので、flatmap はない。


    // 演習
    // Map[String, String] を、 User にマッピングする
    /*
    the name and age must be specified;
    the name must not be blank;
    the the age must be a valid non-negative integer.
     */
    println("------------解答-------")
    case class User(name: String, age: Int)
    type ErrorOr[A] = Either[List[String], A]

    // 単品のチェックメソッド
    def readName(map:Map[String, String]):ErrorOr[String] = map.get("name") match {
      case Some(x) if x.size > 0 => Right(x)
      case Some(x) => Left(List("name is blank"))
      case _ => Left(List("name is nod specified"))
    }
    def readAge(map:Map[String, String]):ErrorOr[Int] =
      map.get("age").filter(_.matches("^[-+]?[0-9]+$")).map(_.toInt) match {
        case Some(x) if x > 0 => Right(x)
        case Some(x) => Left(List("age is not positive"))
        case _ => Left(List("age is nod specified or not integer"))
      }
    import cats.syntax.either._
    import cats.syntax.apply._
    import cats.instances.list._
    import cats.Cartesian

    def readUser(data: Map[String, String]): AllErrorsOr[User] =
      Cartesian[AllErrorsOr].product(
        readName(data).toValidated,
        readAge(data).toValidated
      ).map(User.tupled)
    /** apply style での解答 */
    def readUser2(data: Map[String, String]): AllErrorsOr[User] = {
      val name:AllErrorsOr[String] = readName(data).toValidated
      val age:AllErrorsOr[Int] = readAge(data).toValidated
      (name, age).mapN(User.apply)
    }


    println(readUser(Map("age"->"23", "name"->"ken")))
    println(readUser2(Map("age"->"-1", "name"->"")))
  }
  /** formバリデーションの教科書解答。すごい */
  def validateAnswer():Unit = {

    println("------------解答222-------")
    import cats.data.Validated
    type FormData = Map[String, String]
    type ErrorsOr[A] = Either[List[String], A]
    type AllErrorsOr[A] = Validated[List[String], A]

    // 汎用 map 取得
    def getValue(name: String)(data: FormData): ErrorsOr[String] =
      data.get(name).
        toRight(List(s"$name field not specified"))

    // なるほど。
    val getName = getValue("name") _
    val getAge = getValue("age") _

    println(getName(Map()))

    type NumFmtExn = NumberFormatException
    import cats.syntax.either._
     def parseInt(name: String)(data: String): ErrorsOr[Int] =
      Right(data) // catchOnlyは、catsのsyntax拡張。 Rightに一旦囲んで変換してくの参考になる。
       .flatMap(s => Either.catchOnly[NumFmtExn](s.toInt)) // 例外が発声したら leftへ。
        .leftMap(_ => List(s"$name must be an integer")) //失敗時はメッセージ設定

    println(parseInt("age")("11")) // Right
    println(parseInt("age")("hoge")) // Left
    // ヘルパー。
    def nonBlank(name: String)(data: String): ErrorsOr[String] =
      // 動かないので直す。Right(data).ensure(List(s"$name cannot be blank"))(_.nonEmpty) // 条件を満たさない場合、 ensuerの第一引数に移る
      data match {
        case x if x.nonEmpty => Right(data)
        case _ => Left(List(s"$name cannot be blank"))
      }

    def nonNegative(name: String)(data: Int): ErrorsOr[Int] =
      //Right(data).ensure(List(s"$name must be non-negative"))(_ >= 0)
      data match {
        case x if x >= 0 => Right(data)
        case _ => Left(List(s"$name must be non-negative"))
      }

    def readName(data: FormData): ErrorsOr[String] =
      getValue("name")(data).
        flatMap(nonBlank("name"))
    // ここの flatMapでチェック連携してくのすごいな。
    def readAge(data: FormData): ErrorsOr[Int] =
      getValue("age")(data).
        flatMap(nonBlank("age")). //どこかで Leftに落ちたらチェックされない。もししたいなら、ここでも validated使う。
        flatMap(parseInt("age")).
        flatMap(nonNegative("age"))

    import cats.syntax.cartesian._
    import cats.instances.list._

    case class User(name: String, age: Int)
    def readUser(data: FormData): AllErrorsOr[User] = {
      val name:AllErrorsOr[String] = readName(data).toValidated
      val age:AllErrorsOr[Int] = readAge(data).toValidated
      (name |@| age).map(User.apply)
    }

    println(readUser(Map("age"->"23", "name"->"ken")))
    println(readUser(Map("age"->"-1", "name"->"")))
  }

  pp()
  validate()
  validateAnswer()

}
