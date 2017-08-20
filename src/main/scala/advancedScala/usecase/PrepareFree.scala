package advancedScala.usecase

import cats.free.Free


/**
  * Created by kentaro.maeda on 2017/08/18.
  */
object PrepareFree extends App{

  // ADTを用意
  sealed trait KVStoreA[A]
  case class Put[T](key: String, value: T) extends KVStoreA[Unit]
  case class Get[T](key: String) extends KVStoreA[Option[T]]
  case class Delete(key: String) extends KVStoreA[Unit]

  import cats.free.Free
  import cats.free.Free.liftF

  // Stateのように、ADTを Freeでくるむ。
  type KVStore[A] = Free[KVStoreA, A]

  // Put returns nothing (i.e. Unit).
  // liftFは、ADTを Freeに上げる。
  // 各関数の結果を、ADTの各型にマッピングする。
  def put[T](key: String, value: T): KVStore[Unit] =
    liftF[KVStoreA, Unit](Put[T](key, value))

  // Get returns a T value.
  def get[T](key: String): KVStore[Option[T]] =
    liftF[KVStoreA, Option[T]](Get[T](key))

  // Delete returns nothing (i.e. Unit).
  def delete(key: String): KVStore[Unit] =
    liftF(Delete(key))

  // Update composes get and set, and returns nothing.
  def update[T](key: String, f: T => T): KVStore[Unit] =
    for {
      vMaybe <- get[T](key)
      _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
    } yield ()

  // 実際のプログラム。メソッド呼び出しが、Freeインスタンスのネストに変わる。
  def program: KVStore[Option[Int]] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", (_ + 12))
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n

  println(program) // Free(...)

  // programを解釈して実行するインタプリタを定義する。
  // インタプリタの内容次第で色々結果を作れる。
  import cats.arrow.FunctionK
  import cats.{Id, ~>}
  // ~> は FunctionK, FuntcionK は、 M[_] => M'[_] のように、モナドに対する変換を定義する。
  // 今回は単なる値A が欲しいので、Idだが、Futurer とか色々できる。
  import scala.collection.mutable

  // the program will crash if a key is not found,
  // or if a type is incorrectly specified.
  def impureCompiler: KVStoreA ~> Id  = new (KVStoreA ~> Id) {

    // a very simple (and imprecise) key-value store
    val kvs = mutable.Map.empty[String, Any]
    // ADTの型に応じたプログラムを書いていける。
    def apply[A](fa: KVStoreA[A]): Id[A] = //applyの戻り値が、 ~>の右辺のIdになる。
      fa match {
        case Put(key, value) =>
          println(s"put($key, $value)")
          kvs(key) = value
          ()
        case Get(key) =>
          println(s"get($key)")
          kvs.get(key)
        case Delete(key) =>
          println(s"delete($key)")
          kvs.remove(key)
          ()
      }
  }

  // インタプリターを programに対してfoldMapで指定すると実行。
  val result: Option[Int] = program.foldMap(impureCompiler)
  println(result)

  // 複数のインタプリターを切り替えできる。
  import cats.data.State

  type KVStoreState[A] = State[Map[String, Any], A]
  val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {
    def apply[A](fa: KVStoreA[A]): KVStoreState[A] =
      fa match {
        case Put(key, value) => State.modify(_.updated(key, value))
        case Get(key) =>
          State.inspect(_.get(key).map(_.asInstanceOf[A]))
        case Delete(key) => State.modify(_ - key)
      }
  }
  val result2: (Map[String, Any], Option[Int]) = program.foldMap(pureCompiler).run(Map.empty).value
  println(result2)
}
