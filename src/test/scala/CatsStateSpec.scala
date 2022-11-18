import cats.data.State
import munit.FunSuite

class CatsStateSpec extends FunSuite {

  val getDemo = State.get[Int]

  test("State") {
      // val step1 = State[Int, String]{}
  }
}
