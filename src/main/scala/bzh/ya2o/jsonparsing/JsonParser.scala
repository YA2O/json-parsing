package bzh.ya2o.jsonparsing

import bzh.ya2o.jsonparsing.Json._

import scala.util.parsing.combinator._

object JsonParser extends JavaTokenParsers {

  def apply(input: String): Option[Json] = parseAll(obj, input) match {
    case Success(result, _) ⇒ Some(result)
    case NoSuccess(_, _) ⇒ None
  }

  private[this] def obj: Parser[Json] =
    "{" ~> repsep(member, ",") <~ "}" ^^ { m: List[(String, Json)] ⇒ JsonObj(m.toMap) }

  private[this] def arr: Parser[Json] = {
    "[" ~> repsep(value, ",") <~ "]" ^^ { l: List[Json] ⇒ JsonArr(l) }
  }

  private[this] def member: Parser[(String, Json)] =
    stringLiteral ~ ":" ~ value ^^ {
      case name ~ ":" ~ value ⇒ (removeLeadingAndTailingQuotes(name), value)
    }

  private[this] def value: Parser[Json] = (
    obj
      | arr
      | floatingPointNumber ^^ { s ⇒ JsonNumber(s.toDouble) }
      | stringLiteral ^^ { s ⇒ JsonString(removeLeadingAndTailingQuotes(s)) }
      | "true" ^^ { _ ⇒ JsonBoolean(true) }
      | "false" ^^ { _ ⇒ JsonBoolean(false) }
      | "null" ^^ { _ ⇒ JsonNull }
    )

  private[this] def removeLeadingAndTailingQuotes(s: String) = s.substring(1, s.length - 1)
}
