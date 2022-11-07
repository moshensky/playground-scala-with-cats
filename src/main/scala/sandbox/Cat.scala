package sandbox
import printable.PrintableInstances._
import printable.Printable
import cats.Show
import cats.kernel.Eq

final case class Cat(name: String, age: Int, color: String)

object Cat {
  private val showStr = Show.apply[String]
  private val showInt = Show.apply[Int]
  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {

    override def format(cat: Cat): String = {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"${name} is a ${age} old ${color} cat."
    }

  }

  implicit val showCat: Show[Cat] = Show.show(cat => {
    val name = showStr.show(cat.name)
    val age = showInt.show(cat.age)
    val color = showStr.show(cat.color)
    s"${name} is a ${age} old ${color} cat."
  })

  implicit val eqCat: Eq[Cat] = Eq.instance[Cat] { (a, b) =>
    Eq.eqv(a.name, b.name) && Eq.eqv(a.age, b.age) && Eq.eqv(a.color, b.color)
  }
}
