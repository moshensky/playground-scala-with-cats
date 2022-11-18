package circe_auto

import io.circe.generic.auto._
import io.circe.parser
import munit.FunSuite

case class Nested(arrayField: List[Int])
case class OurJson(
    textField: String,
    numericField: Int,
    booleanField: Boolean,
    nestedObject: Nested
)

class CicrceAutoGenEncSpec extends FunSuite {
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

  test("Auto gen enc") {
    val decoded = parser.decode[OurJson](jsonString)
    assertEquals(
      decoded,
      Right(OurJson("textContent", 123, true, Nested(List(1, 2, 3))))
    )
  }
}
