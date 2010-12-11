package net.liftweb.http.rest

import org.specs.runner.{Console, JUnit, Runner}
import org.specs.Specification
import net.liftweb.http._
import net.liftweb.webapptest.JettyTestServer
import testing.RequestKit

class RestHelperSpecTest extends Runner(RestHelperSpec) with JUnit with Console

object RestHelperSpec extends Specification with RequestKit {
  doBeforeSpec(JettyTestServer.start)

  def baseUrl = JettyTestServer.baseUrl

  "RestHelper" should {
    "respond with XML representation for XML request" in {
     val resp = get("/webservices/all_users", theHttpClient, ("Accept", "application/xml") :: Nil)
     resp.open_!.xml.open_! must beEqualTo(<test>success</test>)
    }

    "respond with JSON representation for JSON request" in {
      val resp = get("/webservices/all_users", theHttpClient, ("Accept", "application/json") :: Nil)
      resp.open_!.bodyAsString.open_! must beEqualTo("\"success\"")
    }

    "respond with the representation with highest q factor" in {
      val resp = get("/webservices/all_users", theHttpClient,
        ("Accept", "application/json;q=0.8, application/xml") :: Nil)
      resp.open_!.xml.open_! must beEqualTo(<test>success</test>)
    }

    "respond with 406 status when the client's requested representation is not available at the server" in {
      val resp = get("/webservices/all_users", theHttpClient, ("Accept", "text/plain") :: Nil)
      resp.open_!.code must beEqualTo(406)
    }

    "respond with any available representation when there is no Accept header with client's preference" in {
      val resp = get("/webservices/all_users")
      resp.open_!.contentType must beOneOf("text/xml; charset=utf-8", "application/xml; charset=utf-8",
        "application/json; charset=utf-8")
    }
  }
}
