package net.liftweb.http

import org.specs.Specification
import org.specs.runner.{Console, JUnit, Runner}
import net.liftweb.common.Empty

class ReqSpecTest extends Runner(ReqSpec) with JUnit with Console

object ReqSpec extends Specification {
  val xmlPreferredAcceptHeader = "application/json;q=0.8, application/xml"
  val jsonPreferredAcceptHeader = "application/json, application/xml;q=0.6"
  val orderedMediaTypes = ContentType.parse(xmlPreferredAcceptHeader)

  "Req's ContentType" should {
    "build content types based on Accept header" in {
      orderedMediaTypes must have size(2)
      orderedMediaTypes must beLike {case ContentType(_, _, _, _, _) :: ContentType(_, _, _, _, _) :: Nil => true}
    }

    "have XML first in the list for XML-preferred Accept header" in {
      (orderedMediaTypes head) must beEqualTo(ContentType("application", "xml", 1, Empty, List()))
    }

    "have JSON first in the list for JSON-preferred Accept header" in {
      val orderedMediaTypes = ContentType.parse(jsonPreferredAcceptHeader)
      (orderedMediaTypes head) must beEqualTo(ContentType("application", "json", 0, Empty, List()))
    }

    "order more specific content types first" in {
      val acceptHeader = "*/*, application/json, application/*"
      val orderedMediaTypes = ContentType.parse(acceptHeader)
      orderedMediaTypes must beEqualTo(List(ContentType("application","json",1,Empty,List()),
        ContentType("application","*",2,Empty,List()),
        ContentType("*","*",0,Empty,List())))
    }
  }

}