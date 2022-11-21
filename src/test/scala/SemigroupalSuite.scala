import munit.FunSuite
import cats.Semigroupal
import cats.syntax.apply._
import cats.instances.option._
import cats.Foldable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.Await

class SemigroupalSuite extends FunSuite {
  test("product") {
    assertEquals(
      Semigroupal[Option].product(Some(133), Some("abs")),
      Some((133, "abs"))
    )
    assertEquals(
      Semigroupal[Option].product(Some(133), Option.empty[String]),
      None
    )
    assertEquals((Option(133), Option("xyz")).tupled, Some((133, "xyz")))
  }

  test("map") {
    import cats.syntax.parallel._
    val res = (List(1, 2), List(3, 4)).parTupled
    println(res)
    assertEquals(
      Semigroupal[Option].product(Some(133), Some("abs")),
      Some((133, "abs"))
    )
    assertEquals(
      Semigroupal[Option].product(Some(133), Option.empty[String]),
      None
    )
  }

  test("folds") {
    val ints = List(1, 2, 3)
    assertEquals(Foldable[List].foldLeft(ints, 0)(_ + _), 6)
  }

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.example.com"
  )

  test("fold without traverse") {
    def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60)

    val allUptimes: Future[List[Int]] =
      hostnames.foldLeft(Future(List.empty[Int])) { (acc, x) =>
        val uptime = getUptime(x)
        acc.flatMap(y => uptime.map(z => y :+ z))
      }

    val res = Await.result(allUptimes, 1.second)
    assertEquals(res, List(1020, 960, 1020))
  }

  test("traverse") {
    def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60)
    val allUptimes: Future[List[Int]] = Future.traverse(hostnames)(getUptime)
    val res = Await.result(allUptimes, 1.second)
    assertEquals(res, List(1020, 960, 1020))
  }

}
