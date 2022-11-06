package sandbox

import json.{JsonWriter, Json, JsObject, JsString}

final case class Person(name: String, email: String)

object Person {
  implicit val personWriter: JsonWriter[Person] = new JsonWriter[Person] {

    override def write(value: Person): Json = JsObject(
      Map(
        "name" -> JsString(value.name),
        "email" -> JsString(value.email)
      )
    )

  }

}
