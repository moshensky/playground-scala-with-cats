import munit.FunSuite
import cats.Semigroup

final case class CheckF[E, A](f: A => Either[E, A]) {
  def apply(a: A): Either[E, A] = f(a)

  def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
    CheckF[E, A] { a =>
      (this(a), that(a)) match {
        case (Left(e1), Left(e2)) => Left(Semigroup[E].combine(e1, e2))
        case (Left(e), Right(_))  => Left(e)
        case (Right(_), Left(e))  => Left(e)
        case (Right(_), Right(_)) => Right(a)
      }
    }
}

sealed trait Check[E, A] {
  import Check._
  def and(that: Check[E, A]): Check[E, A] = And(this, that)
  def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] = this match {
    case And(left, right) =>
      (left(a), right(a)) match {
        case (Left(e1), Left(e2)) => Left(s.combine(e1, e2))
        case (Left(e), Right(_))  => Left(e)
        case (Right(_), Left(e))  => Left(e)
        case (Right(_), Right(_)) => Right(a)
      }
    case Pure(f) => f(a)
  }
}

object Check {
  final case class And[E, A](left: Check[E, A], right: Check[E, A])
      extends Check[E, A]
  final case class Pure[E, A](f: A => Either[E, A]) extends Check[E, A]

  def pure[E, A](f: A => Either[E, A]) = Pure(f)
}

class Ch10DataValidationSuite extends FunSuite {
  test("CheckF") {
    val a = CheckF[List[String], Int] { x =>
      if (x > 2) Right(x) else Left(List("Must be > 2"))
    }
    val b = CheckF[List[String], Int] { x =>
      if (x < -2) Right(x) else Left(List("Must be < -2"))
    }
    val check = a and b
    assertEquals(check(5), Left(List("Must be < -2")))
    assertEquals(check(0), Left(List("Must be > 2", "Must be < -2")))
  }

  test("Check") {
    val a: Check[List[String], Int] = Check.pure { x =>
      if (x > 2) Right(x) else Left(List("Must be > 2"))
    }
    val b = Check.pure[List[String], Int] { x =>
      if (x < -2) Right(x) else Left(List("Must be < -2"))
    }
    val check = a and b
    assertEquals(check(5), Left(List("Must be < -2")))
    assertEquals(check(0), Left(List("Must be > 2", "Must be < -2")))
  }
}
