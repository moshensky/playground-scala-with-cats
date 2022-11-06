package sandbox

// import cats.implicits._
import json.Json
import json.JsonWriter
import json.JsonSyntax._
import json.JsonWriterInstances._
import printable.Printable
import printable.PrintableSyntax._

import cats.Show
import cats.syntax.show._
// import cats.instances.int._
import java.util.Date


object Main extends App {
  // println("Hello " |+| "Cats!")
  val dave = Person("Dave", "dave@example.com")
  println(dave)

  println(Json.toJson(dave))
  println(dave.toJson)
  val whatAmI = implicitly[JsonWriter[String]]
  println(whatAmI)
  val optionJson = Option("I'm an option").toJson
  // val optionJson = Some("I'm an option").toJson
  println(optionJson)

  val cat = Cat("Kasmi", 1, "striped")
  Printable.print(cat)
  cat.print

  val showInt = Show.apply[Int]
  val showStr = Show.apply[String]
  println(showInt.show(7))
  println(showStr.show("xxx"))

  implicit val dateShow0: Show[Date] = Show.show(x => s"${x.getTime}ms since the epoch.")
  println(new Date().show)
}
