package circe_auto

import org.scalatest.wordspec.AnyWordSpec
import io.circe.generic.auto._
import io.circe.parser

case class Nested(arrayField: List[Int])
case class OurJson(
    textField: String,
    numericField: Int,
    booleanField: Boolean,
    nestedObject: Nested
)

class CicrceAutoGenEncSpec extends AnyWordSpec {
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

  "Auto gen enc" when {
    "using crice" in {
      val decoded = parser.decode[OurJson](jsonString)
      assert(
        decoded === Right(
          OurJson("textContent", 123, true, Nested(List(1, 2, 3)))
        )
      )
    }
  }
}
