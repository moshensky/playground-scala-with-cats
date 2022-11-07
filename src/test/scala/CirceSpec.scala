import org.scalatest.wordspec.AnyWordSpec
import io.circe._
import io.circe.parser._
import io.circe.generic.semiauto._
import io.circe.syntax._

case class Nested(arrayField: List[Int])
case class OurJson(
    textField: String,
    numericField: Int,
    booleanField: Boolean,
    nestedObject: Nested
)

class CirceSpec extends AnyWordSpec {
  implicit val nestedDecoder: Decoder[Nested] = deriveDecoder[Nested]
  implicit val jsonDecoder: Decoder[OurJson] = deriveDecoder[OurJson]

  implicit val nestedEncoder: Encoder[Nested] = deriveEncoder[Nested]
  implicit val jsonEncoder: Encoder[OurJson] = deriveEncoder[OurJson]

  val jsonString = """
    |{
    | "textField": "textContent",
    | "numericField": 123,
    | "booleanField": true,
    | "nestedObject": {
    | "arrayField": [1, 2, 3]
    | }
    |}
    |""".stripMargin

  "Validating JSON" when {
    "xxx" should {
      "validate a JSON string" in {
        val parseResult: Either[ParsingFailure, Json] = parse(jsonString)
        val xxx = parseResult match {
          case Left(value) =>
            throw new IllegalArgumentException(
              s"Invalid JSON object: ${value.message}"
            )
          case Right(json) =>
            val numbers = json \\ "numericField"
            val firstNumber = numbers.collectFirst({ case field =>
              field.asNumber
            })
            val singleOption = firstNumber.flatten.flatMap(_.toInt)
            singleOption
        }
        assert(Some(123) === xxx)
      }

      "decode" in {
        val decoded = decode[OurJson](jsonString)
        assert(
          decoded === Right(
            OurJson("textContent", 123, true, Nested(List(1, 2, 3)))
          )
        )
      }

      "encode" in {
        val data = OurJson("textContent", 123, true, Nested(List(1, 2, 3)))
        val jsonObj: Json = data.asJson
        val newJsonString = jsonObj.spaces2
        println(newJsonString)
      }
    }
  }
}
