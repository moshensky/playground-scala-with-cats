import cats.data.State
import munit.FunSuite

class CatsStateSpec extends FunSuite {

  type CalcState[A] = State[List[Int], A]

  def someTransformation(num: Int): CalcState[Int] = State { oldStack =>
    (num :: oldStack, num)
  }
  def someCalculation(f: (Int, Int) => Int): CalcState[Int] = State {
    case b :: a :: tail => (tail, f(a, b))
    case _              => ???
  }

  def evalOne(sym: String): CalcState[Int] = {
    sym match {
      case "+" => someCalculation(_ + _)
      case x   => someTransformation(x.toInt)
    }
  }

  test("State") {
    val step1 = State[Int, String] { num =>
      val ans = num * 2
      (ans, s"Result of step1: $ans")
    }

    val step2 = State[Int, String] { num =>
      val ans = num * 2
      (ans, s"Result of step2: $ans")
    }

    val both = for {
      a <- step1
      b <- step2
    } yield (a, b)

    val (state, result) = both.run(20).value
    println(state)
    println(result)
  }

  test("evalOne") {
    assertEquals(evalOne("42").runA(Nil).value, 42)
  }

  test("evalOne with op") {
    val program = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    } yield ans

    assertEquals(program.runA(Nil).value, 3)
  }
}
