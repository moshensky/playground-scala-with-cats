import munit.FunSuite
import cats.Monoid
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import cats.syntax.all._

trait MyFoldable {
  def foldMap[A, B: Monoid](xs: Vector[A])(f: A => B): B
  def parFoldMap[A, B: Monoid](values: Vector[A])(f: A => B): Future[B]
}

class TestMyFoldable extends MyFoldable {

  override def parFoldMap[A, B: Monoid](values: Vector[A])(
      f: A => B
  ): Future[B] = {
    val cpuCount = Runtime.getRuntime.availableProcessors
    val grouped = values.grouped(cpuCount).toList
    grouped.map(xs => Future(foldMap(xs)(f))).sequence.map(Monoid[B].combineAll)
  }

  override def foldMap[A, B: Monoid](xs: Vector[A])(f: A => B): B = {
    val result = xs.map(f).foldLeft(Monoid[B].empty)(Monoid[B].combine)
    return result
  }

}

class Ch9MapReduceSuite extends FunSuite {
  test("MyFoldable") {
    val tmf = new TestMyFoldable()
    val result: Int = tmf.foldMap(Vector(1, 2, 3))(identity)
    assertEquals(result, 6)

    assertEquals(
      tmf.foldMap("Hello world!".toVector)(_.toString.toUpperCase),
      "HELLO WORLD!"
    )

    assertEquals(tmf.foldMap(Vector(1, 2, 3))(_.toString + "! "), "1! 2! 3! ")
  }

  test("num cpus") {
    val result = Runtime.getRuntime.availableProcessors
    assertEquals(result, 8)
  }

  test("parFoldMap") {
    val tmf = new TestMyFoldable()
    val result = tmf.parFoldMap((1 to 1000000).toVector)(identity)
    assertEquals(Await.result(result, 1.second), 1784293664)
  }
}
