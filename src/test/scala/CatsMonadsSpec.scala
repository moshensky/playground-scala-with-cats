import cats.MonadError
import scala.util.Success
import scala.util.Try
import munit.FunSuite

class CatsMonadsSpec extends FunSuite {
  def validateAdult[F[_]](age: Int)(implicit
      me: MonadError[F, Throwable]
  ): F[Int] = if (age >= 18) me.pure(age)
  else me.raiseError(new IllegalArgumentException())

  test("xxx") {
    assertEquals(validateAdult[Try](18), Success(18))
  }
}
