package sandbox

import cats.implicits._
import json.Json
import json.JsonWriter
import json.JsonSyntax._
import json.JsonWriterInstances._

object Main extends App {
  println("Hello " |+| "Cats!")
  val dave = Person("Dave", "dave@example.com")
  println(dave)

  println(Json.toJson(dave))
  println(dave.toJson)
  val whatAmI = implicitly[JsonWriter[String]]
  println(whatAmI)
  val optionJson = Option("I'm an option").toJson
  // val optionJson = Some("I'm an option").toJson
  println(optionJson)
}
