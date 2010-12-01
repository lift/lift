/*
 * Copyright 2010 WorldWide Conferencing, LLC
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
package snippet {

import _root_.net.liftweb._
import http._
import util._
import js._
import JsCmds._
import JE._

import _root_.scala.xml._

/**
 * A demo of the LiftSession.addPostPageJavaScript feature.
 * This snippet sets the innerHTML of the specified ID
 * to the current time and the count of how
 * many times the message was updated.
 */
object Timely {
  def render(in: NodeSeq): NodeSeq = {
    var x = 0

    for {
      theId <- (in \ "@id")
      session <- S.session
    } {
      session.addPostPageJavaScript(() => {
        x += 1
        SetHtml(theId.text,
                <span>The time of the last update to
                this page is <b>{Helpers.now.toString}</b>,
                and this section was updated {x} times.
              </span>)
      })
    }

    in
  }
}
}
}
}
