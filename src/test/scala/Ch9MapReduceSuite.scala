import munit.FunSuite
import cats.Monoid

trait MyFoldable {
  def foldMap[A, B: Monoid](xs: Vector[A])(f: A => B): B
}

class TestMyFoldable extends MyFoldable {

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
}
