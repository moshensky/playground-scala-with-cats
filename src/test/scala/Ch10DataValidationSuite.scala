import munit.FunSuite
import cats.Semigroup
import cats.data.Validated
import cats.data.Validated._
import cats.syntax.apply._

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

sealed trait CheckT[E, A] {
  import CheckT._
  def and(that: CheckT[E, A]): CheckT[E, A] = And(this, that)

  def or(that: CheckT[E, A]): CheckT[E, A] = Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Or(left, right) =>
      (left(a), right(a)) match {
        case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
        case _                          => Valid(a)
      }
    case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
    case Pure(f)          => f(a)
  }
}

object CheckT {
  final case class And[E, A](left: CheckT[E, A], right: CheckT[E, A])
      extends CheckT[E, A]
  final case class Or[E, A](left: CheckT[E, A], right: CheckT[E, A])
      extends CheckT[E, A]
  final case class Pure[E, A](f: A => Validated[E, A]) extends CheckT[E, A]

  def pure[E, A](f: A => Validated[E, A]) = Pure(f)
}

sealed trait Predicate[E, A] {
  import Predicate._
  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)
  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
    case Pure(f)          => f(a)
    case Or(left, right) =>
      (left(a), right(a)) match {
        case (Invalid(e1), Invalid(e2)) => Invalid(s.combine(e1, e2))
        case _                          => Valid(a)
      }
  }
}

object Predicate {
  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A])
      extends Predicate[E, A]
  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A])
      extends Predicate[E, A]
  final case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]

  def pure[E, A](f: A => Validated[E, A]) = Pure(f)
}

sealed trait Check[E, A, B] {
  import Check._

  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] = Map[E, A, B, C](this, f)

}

object Check {
  private final case class Map[E, A, B, C](check: Check[E, A, B], f: B => C)
      extends Check[E, A, C] {

    override def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] = check(in).map(f)

  }

  private final case class Pure[E, A](pred: Predicate[E, A])
      extends Check[E, A, A] {

    override def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] = pred(in)

  }

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = Pure(pred)
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
    val a: CheckT[List[String], Int] = CheckT.pure { x =>
      if (x > 2) Valid(x) else Invalid(List("Must be > 2"))
    }
    val b = CheckT.pure[List[String], Int] { x =>
      if (x < -2) Valid(x) else Invalid(List("Must be < -2"))
    }
    val check = a and b
    assertEquals(check(5), Invalid(List("Must be < -2")))
    assertEquals(check(0), Invalid(List("Must be > 2", "Must be < -2")))
  }

  test("Check or") {
    val a: CheckT[List[String], Int] = CheckT.pure { x =>
      if (x > 2) Valid(x) else Invalid(List("Must be > 2"))
    }
    val b = CheckT.pure[List[String], Int] { x =>
      if (x < -2) Valid(x) else Invalid(List("Must be < -2"))
    }
    val check = a or b
    assertEquals(check(5), Valid(5))
    assertEquals(check(0), Invalid(List("Must be > 2", "Must be < -2")))
  }
}
