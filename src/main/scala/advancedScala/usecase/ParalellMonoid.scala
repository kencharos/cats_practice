package advancedScala.usecase

import java.util.concurrent.Executors

import scala.concurrent.Await


/**
  * Created by kentaro.maeda on 2017/08/18.
  */
object ParalellMonoid extends App{

  import cats.implicits._
  import cats.Monoid
  // ex
  //a sequence of type Vector[A];
  // a function of type A => B, where there is a Monoid for B;
  def foldMap[A,B : Monoid](vector: Vector[A])(f: A => B):B = {
    vector.foldLeft(Monoid[B].empty)((b, a) => Monoid[B].combine(b, f(a)))
  }

  println(foldMap(Vector(1,2,3,4))(_.toString))
  println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))

  import scala.concurrent.Future
  import scala.concurrent.duration._
  //implicit val ex =  scala.concurrent.ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  import scala.concurrent.ExecutionContext.Implicits.global
  //
  def parallelFoldMap[A, B : Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val cpu = Runtime.getRuntime.availableProcessors
    val splited = values.grouped(values.size/cpu)
    val parB = splited.map{v =>  Future(foldMap(v)(func)).map(a => {println(s"start${Thread.currentThread()}");a;})}
    Future.sequence(parB).map(_.foldLeft(Monoid[B].empty)(_ |+| _) )
  }

  // in cats
  def parallelFoldMap2[A, B: Monoid] (values: Vector[A])(func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    values
      .grouped(groupSize)
      .toVector // Vector[Iterator[A]]
      .traverse(group => Future(group.toVector.foldMap(func))) // Future[Vector[B]] //IDEAだとエラーになる。
      .map(_.combineAll)  // Futuer[B]
  }

  println(Await.result(parallelFoldMap((1 to 1000000).toVector)(identity), 3.seconds))
  println(Await.result(parallelFoldMap2((1 to 1000000).toVector)(identity), 3.seconds))


}
