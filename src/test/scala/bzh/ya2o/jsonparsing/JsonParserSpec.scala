package bzh.ya2o.jsonparsing

import bzh.ya2o.jsonparsing.Json._

import org.scalatest.{ FlatSpec, Matchers }

class JsonParserSpec extends FlatSpec with Matchers {

  "the JSON parser" should "unmarshall a JSON string" in {
    val inputJsonString =
      """{
        |   "a": "aaa",
        |   "bb": 123,
        |   "C": 234.5,
        |   "D": {
        |     "da": [
        |       "DAA",
        |       null,
        |       true,
        |       false,
        |       {},
        |       []
        |     ],
        |     "db": {},
        |     "dc": []
        |   }
        | }""".stripMargin

    val jsonOpt: Option[Json] = JsonParser(inputJsonString)
    println(jsonOpt.get)

    val expected = JsonObj(Map(
      "a" -> JsonString("aaa"),
      "bb" -> JsonNumber(123.0),
      "C" -> JsonNumber(234.5),
      "D" -> JsonObj(
        Map(
          "da" -> JsonArr(List(
            JsonString("DAA"),
            JsonNull,
            JsonBoolean(true),
            JsonBoolean(false),
            JsonObj(Map()),
            JsonArr(List())
          )),
          "db" -> JsonObj(Map()),
          "dc" -> JsonArr(List())))
    ))

    jsonOpt.get should === (expected)
  }

  it should "marshall a POSO (Plain Old Scala Object)" in {
    val poso = Poso("AA")
    val poso2=for {
      json <- JsonParser("""{"stuff": "AA"}""")
      stuff <- json.getProperty[JsonString]("stuff")
    } yield Poso(stuff.value)

    poso2.get should === (poso)
  }



}
  case class Poso(stuff: String)
