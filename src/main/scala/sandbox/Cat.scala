package sandbox
import printable._

final case class Cat(name: String, age: Int, color: String)

object Cat {
  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {

    override def format(value: Cat): String =
      s"${value.name} is a ${value.age} old ${value.color} cat."

  }
}
