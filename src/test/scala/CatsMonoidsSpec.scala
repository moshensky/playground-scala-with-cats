import org.scalatest.wordspec.AnyWordSpec
import cats.Monoid
import cats.syntax.semigroup._

case class Order(totalCost: Double, quantity: Double)

object Order {
  implicit val orderM = new Monoid[Order] {

    override def combine(x: Order, y: Order): Order =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)

    override def empty: Order = Order(0, 0)

  }
}

class CatsMonoidSpec extends AnyWordSpec {
  "Monoid laws for boolean and" should {
    "combine" in {
      assert(Monoid[String].combine("Hi ", "there") === "Hi there")
    }

    "combine int" in {
      assert(Monoid[Int].combine(32, 10) === 42)
    }

    "combine options" in {
      assert(Monoid[Option[Int]].combine(Option(22), Some(20)) === Some(42))
    }

    "combine syntax" in {
      assert(("Hi " |+| "there" |+| Monoid[String].empty) === "Hi there")
    }

    "empty" in {
      assert(Monoid[String].empty === "")
    }

  }

  "SuperAdder's" should {

    def add[A](xs: List[A])(implicit m: Monoid[A]): A = xs.reduce(m.combine)

    "add" in {
      assert(add(List(1, 3, 5)) === 9)
    }

    "add another" in {
      assert(add(List(1)) === 1)
    }

    "order" in {
      assert(add(List(Order(1, 1), Order(1, 1))) === Order(2, 2))
    }
  }

}
