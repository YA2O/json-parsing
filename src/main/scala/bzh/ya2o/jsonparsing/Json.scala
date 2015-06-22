package bzh.ya2o.jsonparsing


//import scala.util.parsing.combinator.Parsers.NoSuccess
//import scala.util.parsing.combinator.Parsers.Parser
//import scala.util.parsing.combinator.Parsers.Success
//import scala.util.parsing.combinator.Parsers.~

object Json {

  import scala.reflect.ClassTag

  sealed abstract class Json extends Serializable {
    def getProperty[T <: Json : ClassTag](key: String): Option[T] = None

    def toPrettyString: String
  }

  case class JsonObj(properties: Map[String, Json]) extends Json {
    override def getProperty[T <: Json : ClassTag](key: String): Option[T] = {
      //			val clazz = implicitly[ClassTag[T]].runtimeClass
      properties.get(key) match {
        case Some(t: T) ⇒ Some(t)
        case _ ⇒ None
      }
    }

    override def toPrettyString: String = "{" +
      properties.map { case (key, value) ⇒
        s""""$key": ${value.toPrettyString}"""
      }.mkString(", ") +
      "}"

  }

  case class JsonArr(elements: List[Json]) extends Json {
    override def toPrettyString: String = s"[${elements.map(_.toPrettyString).mkString(", ")}]"
  }

  case class JsonString(value: String) extends Json {
    override def toPrettyString: String = s""""$value""""
  }

  case class JsonNumber(value: Number) extends Json {
    override def toPrettyString: String = value.toString
  }

  case class JsonBoolean(value: Boolean) extends Json {
    override def toPrettyString: String = value.toString
  }

  case object JsonNull extends Json {
    override def toPrettyString: String = "null"
  }

}
