import munit.FunSuite
import cats.Monad
import cats.data.OptionT

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)
}

class CustomMonadsSuite extends FunSuite {
  val treeMonad = new Monad[Tree] {

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => Tree.branch(flatMap(left)(f), flatMap(right)(f))
      case Leaf(value) => f(value)
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A,B]]): Tree[B] = flatMap(f(a)) { 
      case Left(value) => tailRecM(value)(f)
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
}
