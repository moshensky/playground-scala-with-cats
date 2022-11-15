import org.scalatest.wordspec.AnyWordSpec
import cats.MonadError
import scala.util.Success
import scala.util.Try

class CatsMonadsSpec extends AnyWordSpec {
  def validateAdult[F[_]](age: Int)(implicit
      me: MonadError[F, Throwable]
  ): F[Int] = if (age >= 18) me.pure(age) else  me.raiseError(new IllegalArgumentException())

  "Monads" should {
    "xxx" in {
      assert(validateAdult[Try](18) === Success(18))
      ///
    }
  }
}
