/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package example {
package comet {

import net.liftweb.actor._
import scala.xml.{NodeSeq, Text}

import net.liftweb._
import http._
import js._
import JsCmds._

import common._
import util._
import Helpers._

case class BuildStatus(progress: Int, url: Box[String])

// a singleton that builds a "thing"
object ThingBuilder extends LiftActor {
  def boot() {
    LiftRules.dispatch.append {
      case Req("getit":: Nil, _, GetRequest) =>
        () => Full(XmlResponse(<info>Here's some info</info>))
    }
  }

  protected def messageHandler =
    {
      case a: LiftActor =>
        this ! (a, 1)

      case (a: LiftActor, x: Int) if x >= 10 =>
        a ! BuildStatus(100, Full("/getit"))

      case (a: LiftActor, i: Int) =>
        a ! BuildStatus(i * 10, Empty)
        ActorPing.schedule(this, (a, i + 1), 2 seconds)

      case _ =>
    }
  
}

// A CometActor that keeps the user updated
class LongTime extends CometActor {
  private var url: Box[String] = Empty
  private var progress: Int = 0

  // a CometActor that has not been displayed for
  // 2 minutes is destroyed
  override def lifespan: Box[TimeSpan] = Full(2 minutes)

  // get messages from the ThingBuilder
  override def highPriority = {
    case BuildStatus(p, Empty) =>
      this.progress = p
      reRender(false)

    case BuildStatus(_, Full(u)) =>
      url = Full(u)
      progress = 100
      reRender(false)
      partialUpdate(RedirectTo(u))
  }

  // start the job
  override def localSetup() {
    ThingBuilder ! this
    super.localSetup()
  }

  // display the progress or a link to the result
  def render =
  url match {
    case Full(where) =>
      <span>Your job is complete.  <a href={where}>Click Me</a></span>
    case _ =>
      <span>We're working on your job... it's {progress}% complete</span>
  }
}
}
}
}

