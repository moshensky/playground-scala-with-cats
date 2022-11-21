import munit.FunSuite
import scala.concurrent.Future
import cats.Id
import cats.Applicative
import cats.syntax.all._

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)

}

class RealUptimeClient extends UptimeClient[Future] {
  override def getUptime(hostname: String): Future[Int] = ???
}

class TestUptimeClient(val hostsMap: Map[String, Int])
    extends UptimeClient[Id] {

  override def getUptime(hostname: String): Int = hostname match {
    case "host1" => 10
    case "host2" => 6
  }

}

class Ch8TestingAsyncCodeSuite extends FunSuite {
  test("UptimeService") {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assertEquals(actual, expected)
  }
}
