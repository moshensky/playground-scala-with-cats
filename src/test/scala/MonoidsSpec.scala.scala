import munit.FunSuite

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) = monoid

  implicit val boolAndM = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
    override def empty: Boolean = true
  }

  implicit val boolOrM = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
    override def empty: Boolean = false
  }

  implicit val boolEitherM = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean =
      (!x && y) || (x && !y)
    // true, false =  (false && false) ||  (true && true) = false || true = true
    // false, true =   (true && true) ||  (false && false) = true || false = true
    // false, false =  (true && false) || (false && true) = false || false = false
    // true, true =   (false && true) ||   (true && false) = false || true = true
    override def empty: Boolean = false
  }

  implicit val boolXnorM = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean =
      (!x || y) && (x || !y)
    // true, false = (false || false) && (true || true) = false && true = false
    // false, true = (true || true) && (false || false) = true && false = false
    // false, false = (true || false) && (false || true) = true && true = true
    // true, true = (false || true) && (true || false) = true && true = true
    override def empty: Boolean = true
  }
}

class MonoidSpec extends FunSuite {

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }

  test("Monoid laws for boolean and: associative") {
    import Monoid.boolAndM
    assert(associativeLaw(true, false, true))
  }

  test("Monoid laws for boolean and: indentity") {
    import Monoid.boolAndM
    assert(identityLaw(true))
    assert(identityLaw(false))
  }

  test("Monoid laws for boolean or: associative") {
    import Monoid.boolOrM
    assert(associativeLaw(true, false, true))
  }

  test("Monoid laws for boolean or: indentity") {
    import Monoid.boolOrM
    assert(identityLaw(true))
    assert(identityLaw(false))
  }

  test("Monoid laws for boolean either: associative") {
    import Monoid.boolEitherM
    assert(associativeLaw(true, false, true))
  }

  test("Monoid laws for boolean either: indentity") {
    import Monoid.boolEitherM
    assert(identityLaw(true))
    assert(identityLaw(false))
  }

  test("Monoid laws for boolean Xnor: associative") {
    import Monoid.boolXnorM
    assert(associativeLaw(true, false, true))
  }

  test("Monoid laws for boolean Xnor: indentity") {
    import Monoid.boolXnorM
    assert(identityLaw(true))
    assert(identityLaw(false))
  }
}
