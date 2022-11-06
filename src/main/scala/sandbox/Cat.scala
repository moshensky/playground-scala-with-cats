package sandbox
import printable.PrintableInstances._
import printable.Printable

final case class Cat(name: String, age: Int, color: String)

object Cat {
  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {

    override def format(cat: Cat): String = {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"${name} is a ${age} old ${color} cat."
    }

  }
}
