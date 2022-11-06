package json

object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] = new JsonWriter[String] {
    def write(value: String): Json = JsString(value)
  }

  implicit def optionWriter[A](implicit
      writer: JsonWriter[A]
  ): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      def write(value: Option[A]): Json = value match {
        case None        => JsNull
        case Some(value) => writer.write(value)
      }
    }
}
