package advancedScala.usecase

import cats.kernel.Monoid

object CRDT2 extends App{

  // A,B,Cごとにカウントし、最大値に寄せるもの。

  // モノイド拡張。結合ではなく、どちらか大きい方を取るもの。
  trait BoundedSemiLattice[A] extends Monoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
  }
  object BoundedSemiLattice {
    implicit object intBoundedSemiLatticeInstance extends
      BoundedSemiLattice[Int] {
      def combine(a1: Int, a2: Int): Int =
        a1 max a2
      val empty: Int = 0
    }
    implicit def setBoundedSemiLatticeInstance[A]:
    BoundedSemiLattice[Set[A]] =
      new BoundedSemiLattice[Set[A]]{
        def combine(a1: Set[A], a2: Set[A]): Set[A] =
          a1 union a2
        val empty: Set[A] = Set.empty[A]
      }
  }

  import cats.instances.list._
  import cats.instances.map._
  import cats.syntax.foldable._
  import cats.syntax.semigroup._

  import scala.language.higherKinds

  //GCounterをさらに抽象化する。MAPを切り替え可能に

  // Trait化。
  trait GCounter[F[_,_],K, V] {
    def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]):
    F[K, V]
    def total(f: F[K, V])(implicit m: Monoid[V]): V
    def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  }

  // Mapに限らず、Mapと似たような性質を持つ CRDTのための型クラスをKVSとして定義する。
  trait KeyValueStore[F[_,_]] {
    def +[K, V](f: F[K, V])(key: K, value: V): F[K, V]
    def get[K, V](f: F[K, V])(key: K): Option[V]
    def getOrElse[K, V](f: F[K, V])(key: K, default: V): V =
      get(f)(key).getOrElse(default)
  }

  object KeyValueStore {
    // syntax
    implicit class KeyValueStoreOps[F[_,_],K, V](f: F[K, V]) {
      def +(key: K, value: V)(implicit kv: KeyValueStore[F]): F[K, V] =
        kv.+(f)(key, value)
      def get(key: K)(implicit kv: KeyValueStore[F]): Option[V] =
        kv.get(f)(key)
      def getOrElse(key: K, default: V)(implicit kv: KeyValueStore
        [F]): V =
        kv.getOrElse(f)(key, default)
    }
    // map版の実装
    implicit object mapKeyValueStoreInstance extends KeyValueStore[Map] {
      def +[K, V](f: Map[K, V])(key: K, value: V): Map[K, V] =
        f + (key, value)
      def get[K, V](f: Map[K, V])(key: K): Option[V] =
        f.get(key)
      override def getOrElse[K, V](f: Map[K, V])(key: K, default:
      V): V =
        f.getOrElse(key, default)
    }
  }

  // 実装作成。
  import cats.Foldable
  import cats.syntax.semigroup._
  import cats.syntax.foldable._
/*
  implicit def keyValueInstance[F[_,_],K, V](
    implicit k: KeyValueStore[F],
          km: Monoid[F[K, V]],
          kf: Foldable[({type l[A]=F[K, A]})#l]): GCounter[F, K, V] =
    new GCounter[F, K, V] {
      import KeyValueStore._ // For KeyValueStore syntax
      def increment(f: F[K, V])(key: K, value: V)(implicit m: Monoid[V]): F[K, V] =
        f + (key, (f.getOrElse(key, m.empty) |+| value))
      def total(f: F[K, V])(implicit m: Monoid[V]): V = f.foldMap(identity _)
      def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] = f1 |+| f2
    }
    */


}
