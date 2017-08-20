package advancedScala.usecase

import cats.kernel.Monoid

object CRDT extends App{

  // A,B,Cごとにカウントし、最大値に寄せるもの。

  final case class GCounterBasic(counters: Map[String, Int]) {
    def increment(machine: String, amount: Int) =
      GCounter(counters + (machine -> (amount + counters.getOrElse
      (machine, 0))))
    def get: Int =
      counters.values.sum
    def merge(that: GCounterBasic): GCounterBasic =
      GCounterBasic(that.counters ++ {
        for((k, v) <- counters) yield { k -> (v max that.counters.getOrElse(k,0))
        }
      })
  }

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

  import scala.language.higherKinds
  import cats.syntax.semigroup._
  import cats.syntax.foldable._
  import cats.instances.map._
  import cats.instances.list._


  final case class GCounter[A](counters: Map[String,A]) {
    def increment(machine: String, amount: A)(implicit m: Monoid[A
      ]) =
      GCounter(counters + (machine -> (amount |+| counters.getOrElse(machine, m.empty))))
    def get(implicit m: Monoid[A]): A = this.counters.values.toList.foldMap(identity)

    def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A
      ]): GCounter[A] =
      GCounter(this.counters |+| that.counters)
  }

  val gc = GCounter[Int](Map("A" -> 0, "B" -> 0))
  val gc2 = GCounter[Int](Map("A" -> 2, "B" -> 0))
  val gc3 = GCounter[Int](Map("A" -> 0, "B" -> 3, "C"->6))

  println(gc)
  println(gc.merge(gc2).merge(gc3))

}
