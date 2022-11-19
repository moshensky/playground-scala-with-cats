import munit.FunSuite
import cats.Monad
import cats.data.OptionT
import cats.data.EitherT
import scala.concurrent.Future
import cats.data.Writer

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)
}

class CustomMonadsSuite extends FunSuite {
  val treeMonad = new Monad[Tree] {

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Branch(left, right) =>
          Tree.branch(flatMap(left)(f), flatMap(right)(f))
        case Leaf(value) => f(value)
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(f(a)) {
        case Left(value)  => tailRecM(value)(f)
        case Right(value) => Tree.leaf(value)
      }

    override def pure[A](x: A): Tree[A] = Tree.leaf(x)

  }

  test("ErrorOrOption") {
    import cats.instances.either._
    import cats.syntax.applicative._
    type ErrorOr[A] = Either[String, A]
    type ErrorOrOption[A] = OptionT[ErrorOr, A]
    val a = 10.pure[ErrorOrOption]
    val b = 32.pure[ErrorOrOption]

    val c = a.flatMap(x => b.map(y => x + y))

    assertEquals(c, 42.pure[ErrorOrOption])

    val ans = for {
      x <- a
      y <- b
    } yield x + y

    assertEquals(ans, 42.pure[ErrorOrOption])
  }

  test("FutureEitherOption") {
    type FutureEither[A] = EitherT[Future, String, A]
    type FutureEitherOption[A] = OptionT[FutureEither, A]

    import cats.instances.future._
    import scala.concurrent.Await
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    import cats.syntax.applicative._

    val futureEitherOr: FutureEitherOption[Int] = for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b

    val result = Await.result(futureEitherOr.value.value, 1.second)
    assertEquals(result, Right(Some(42)))
  }

  test("Logged with transformers") {
    type Logged[A] = Writer[List[String], A]

    def parseNumber(str: String): Logged[Option[Int]] =
      util.Try(str.toInt).toOption match {
        case None        => Writer(List(s"Failed on $str"), None)
        case Some(value) => Writer(List(s"Read $str"), Some(value))
      }

    def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
      import cats.data.OptionT
      val result = for {
        x <- OptionT(parseNumber(a))
        y <- OptionT(parseNumber(b))
        z <- OptionT(parseNumber(c))
      } yield x + y + z

      result.value
    }

    println(addAll("1", "2", "3"))
    println(addAll("1", "a", "3"))
  }

  test("Transformers") {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    import scala.concurrent.Await
    type Response[A] = EitherT[Future, String, A]

    val powerLevels = Map(
      "Jazz" -> 6,
      "Bumblebee" -> 8,
      "Hot Rod" -> 10,
    )

    def getPowerLevel(autobot: String): Response[Int] = {
      val level = powerLevels.get(autobot)
      level match {
        case None => EitherT.left(Future(s"Comms error: $autobot unreachable"))
        case Some(value) => EitherT.right(Future(value))
      }
    }

    val res = Await.result(getPowerLevel("Jazz").value, 1.second)
    assertEquals(res, Right(6))

    val res2 = Await.result(getPowerLevel("xxx").value, 1.second)
    assertEquals(res2, Left("Comms error: xxx unreachable"))

    def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
      for {
        x <- getPowerLevel(ally1)
        y <- getPowerLevel(ally2)
      } yield x + y > 15
    }

    val res3 = Await.result(canSpecialMove("Bumblebee", "Hot Rod").value, 1.second)
    assertEquals(res3, Right(true))

    val res4 = Await.result(canSpecialMove("Bumblebee", "Jazz").value, 1.second)
    assertEquals(res4, Right(false))

    val res5 = Await.result(canSpecialMove("Bumb", "Jazz").value, 1.second)
    assertEquals(res5, Left("Comms error: Bumb unreachable"))
    
    def tacticalReport(ally1: String, ally2: String): String = {
      val res = canSpecialMove(ally1, ally2).value.map({
        case Left(value) => value
        case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
        case Right(false) => s"$ally1 and $ally2 need a recharge."
      })

      Await.result(res, 1.second)
    }

    assertEquals(tacticalReport("Jazz", "Bumblebee"), "Jazz and Bumblebee need a recharge.") 
    assertEquals(tacticalReport("Bumblebee", "Hot Rod"), "Bumblebee and Hot Rod are ready to roll out!")
    assertEquals(tacticalReport("Jazz", "Ironhide"),  "Comms error: Ironhide unreachable")
  }
}
