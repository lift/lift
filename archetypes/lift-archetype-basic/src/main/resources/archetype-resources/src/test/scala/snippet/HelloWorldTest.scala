package ${package} {
package snippet {

import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner
import net.liftweb._
import http._
import net.liftweb.util._
import net.liftweb.common._
import org.specs.matcher._
import org.specs.specification._
import Helpers._
import lib._


class HelloWorldTestSpecsAsTest extends JUnit3(HelloWorldTestSpecs)
object HelloWorldTestSpecsRunner extends ConsoleRunner(HelloWorldTestSpecs)

object HelloWorldTestSpecs extends Specification {
  val session = new LiftSession("", randomString(20), Empty)
  val stableTime = now

  override def executeExpectations(ex: Examples, t: =>Any): Any = {
    S.initIfUninitted(session) {
      DependencyFactory.time.doWith(stableTime) {
        super.executeExpectations(ex, t)
      }
    }
  }

  "HelloWorld Snippet" should {
    "Put the time in the node" in {
      val hello = new HelloWorld
      Thread.sleep(1000) // make sure the time changes

      val str = hello.howdy(<span>Hello at <span id="time">Foo</span></span>).toString

      str.indexOf(stableTime.toString) must be >= 0
      str.indexOf("Hello at") must be >= 0
    }
  }
}

}
}
