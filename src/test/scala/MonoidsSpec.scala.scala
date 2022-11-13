import org.scalatest.wordspec.AnyWordSpec

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) = monoid
}

class MonoidSpec extends AnyWordSpec {

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) === m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }

  "Monoid laws for boolean and" should {
    implicit val boolAndM = new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
      override def empty: Boolean = true
    }

    "associative" in {
      assert(associativeLaw(true, false, true) === true)
    }

    "indentity" in {
      assert(identityLaw(true) === true)
      assert(identityLaw(false) === true)
    }
  }

  "Monoid laws for boolean or" should {
    implicit val boolOrM = new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x || y
      override def empty: Boolean = false
    }

    "associative" in {
      assert(associativeLaw(true, false, true) === true)
    }

    "indentity" in {
      assert(identityLaw(true) === true)
      assert(identityLaw(false) === true)
    }
  }

  "Monoid laws for boolean either" should {
    implicit val boolEitherM = new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = (!x && y) || (x && !y)
      // true, false =  (false && false) ||  (true && true) = false || true = true
      // false, true =   (true && true) ||  (false && false) = true || false = true
      // false, false =  (true && false) || (false && true) = false || false = false
      // true, true =   (false && true) ||   (true && false) = false || true = true
      override def empty: Boolean = false
    }

    "associative" in {
      assert(associativeLaw(true, false, true) === true)
    }

    "indentity" in {
      assert(identityLaw(true) === true)
      assert(identityLaw(false) === true)
    }
  }

  "Monoid laws for boolean Xnor" should {
    implicit val boolXnorM = new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (x || !y)
      // true, false = (false || false) && (true || true) = false && true = false
      // false, true = (true || true) && (false || false) = true && false = false
      // false, false = (true || false) && (false || true) = true && true = true
      // true, true = (false || true) && (true || false) = true && true = true
      override def empty: Boolean = true
    }

    "associative" in {
      assert(associativeLaw(true, false, true) === true)
    }

    "indentity" in {
      assert(identityLaw(true) === true)
      assert(identityLaw(false) === true)
    }
  }
}
