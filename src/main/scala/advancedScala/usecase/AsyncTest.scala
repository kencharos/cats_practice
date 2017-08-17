package advancedScala.usecase

import cats.{Applicative, Monad}

/**
  * Created by kentaro.maeda on 2017/08/17.
  */
object AsyncTest extends App {

  def failCase():Unit = {
    import cats.implicits._
    import scala.concurrent.Future

    // 実際に外部に接続する IF
    trait UptimeClient {
      def getUptime(hostname: String): Future[Int]
    }

    import scala.concurrent.ExecutionContext.Implicits.global
    class UptimeService(client: UptimeClient) {
      def getTotalUptime(hostnames: List[String]): Future[Int] =
        hostnames.traverse(client.getUptime).map(_.sum)
    }
    // モック
    class TestUptimeClient(hosts: Map[String, Int]) extends
      UptimeClient {
      def getUptime(hostname: String): Future[Int] =
        Future.successful(hosts.getOrElse(hostname, 0))
    }

    def testTotalUptime() = {
      val hosts = Map("host1" -> 10, "host2" -> 6)
      val client = new TestUptimeClient(hosts)
      val service = new UptimeService(client)
      val actual = service.getTotalUptime(hosts.keys.toList)
      val expected = hosts.values.sum
      assert(actual == expected) // actual は Future[Int]なので、型が合わないし、Futureの結果も取れないので、expectedをFuturerにしても無駄。
    }

    testTotalUptime()
  }

  //failCase();


  def successCase():Unit = {
    import cats.implicits._
    import cats.Applicative
    import scala.concurrent.Future
    import scala.language.higherKinds

    type Id[A] = A

    // 戻り値を にすると、任意の Mを実装で変えれる。
    // F を Id[A]にすると、 Id[A]は A と同じなので、Mでない値も使える。
    // Idの定義は、 type Id[A] = A であるため。
    // というわけで、各モナドインスタンス(List, Optionなど)が、 F[_] であらわされるので、抽象化できるという強みがある。
    // F[_] で抽象化できるのは、 Monad, Foldable, Applicative など様々で、抽象化すごいみたいな話になる。
    // F[_]が特定ノインスタンスである必要があるなら、 F[_] :Monad みたいに context bound する。
    abstract class UptimeClient[F[_]] {
      def getUptime(hostname: String): F[Int]
    }

    trait RealUptimeClient extends UptimeClient[Future] {
      def getUptime(hostname: String): Future[Int]
    }
    trait TestUptimeClient_ extends UptimeClient[Id] {
      def getUptime(hostname: String): Id[Int]

      // これもOK def getUptime(hostname: String): Int
    }

    class TestUptimeClient(hosts: Map[String, Int])
      extends UptimeClient[Id] {
      def getUptime(hostname: String): Int =
        hosts.getOrElse(hostname, 0)
    }
    // Futureではなく、モナドF による実装になる。traverseを使うためには、Fが Applicativeであることを明示する。
    class UptimeService[F[_]:Applicative](client: UptimeClient[F]) {
      def getTotalUptime(hostnames: List[String]):F[Int] =
        hostnames.traverse(client.getUptime).map(_.sum)
    }
    def testTotalUptime() = {
      val hosts = Map("host1" -> 10, "host2" -> 6)
      val client = new TestUptimeClient(hosts) // ここで型指定が要らないのすごい。
      val service = new UptimeService(client)
      val actual = service.getTotalUptime(hosts.keys.toList)
      val expected = hosts.values.sum
      assert(actual == expected)
    }

    testTotalUptime()
  }

  successCase()

}
