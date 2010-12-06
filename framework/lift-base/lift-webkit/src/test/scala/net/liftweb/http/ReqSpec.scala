package net.liftweb.http

import org.specs.Specification
import org.specs.runner.{Console, JUnit, Runner}
import net.liftweb.common.{Full, Empty}

class ReqSpecTest extends Runner(ReqSpec) with JUnit with Console

object ReqSpec extends Specification {
  def orderedMediaTypes(acceptHeader: String) = ContentType.parse(acceptHeader)

  "Req's ContentType" should {
    "order Accept header's content-type in the decreasing order of q value" in {
      val acceptHeader = "text/plain; q=0.5, text/html, text/x-dvi; q=0.8, text/x-c"
      val orderedTypes = orderedMediaTypes(acceptHeader)
      orderedTypes.head must beEqualTo(ContentType("text", "html", 1, Empty, List()))
      orderedTypes.tail.head must beEqualTo(ContentType("text", "x-c", 3, Empty, List()))
      orderedTypes.init.last must beEqualTo(ContentType("text", "x-dvi", 2, Full(0.8), List()))
      orderedTypes.last must beEqualTo(ContentType("text", "plain", 0, Full(0.5), List()))
    }

    "have XML first in the list for XML-preferred Accept header" in {
      val xmlPreferredAcceptHeader = "application/json;q=0.8, application/xml"
      (orderedMediaTypes(xmlPreferredAcceptHeader) head) must beEqualTo(
        ContentType("application", "xml", 1, Empty, List()))
    }

    "have JSON first in the list for JSON-preferred Accept header" in {
      val jsonPreferredAcceptHeader = "application/json, application/xml; q=0.6"
      (orderedMediaTypes(jsonPreferredAcceptHeader) head) must beEqualTo(
        ContentType("application", "json", 0, Empty, List()))
    }

    "place more specific content types first in order" in {
      val acceptHeader = "*/*, application/json, application/*"
      orderedMediaTypes(acceptHeader) must beEqualTo(List(ContentType("application", "json", 1, Empty, List()),
        ContentType("application", "*", 2, Empty, List()),
        ContentType("*", "*", 0, Empty, List())))
    }

    "if q value is the same use the order in which they are specified" in {
      val acceptHeader = "application/*, text/*"
      orderedMediaTypes(acceptHeader) must beEqualTo(
        List(ContentType("application", "*", 0, Empty, List()), ContentType("text", "*", 1, Empty, List())))
    }

    "order generic media types too in the preferential order based on q value" in {
      val acceptHeader = "application/*;q=0.5, text/*"
      orderedMediaTypes(acceptHeader) must beEqualTo(
        List(ContentType("text", "*", 1, Empty, List()), ContentType("application", "*", 0, Full(0.5), List())))
    }
  }
}