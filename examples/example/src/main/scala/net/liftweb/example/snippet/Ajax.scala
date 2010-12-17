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
package snippet {

import _root_.net.liftweb.http._
import _root_.net.liftweb.widgets.autocomplete._
import S._
import SHtml._
import js._
import js.jquery._
import http.jquery._
import JqJsCmds._
import JsCmds._
import common._
import util._
import Helpers._
import _root_.scala.xml.{Text, NodeSeq}

class Ajax extends Loggable {

  def sample(xhtml: NodeSeq): NodeSeq = {
    // local state for the counter
    var cnt = 0

    // get the id of some elements to update
    val spanName: String = S.attr("id_name") openOr "cnt_id"
    val msgName: String = S.attr("id_msgs") openOr "messages"

    // build up an ajax <a> tag to increment the counter
    def doClicker(text: NodeSeq) =
    a(() => {cnt = cnt + 1; SetHtml(spanName, Text( cnt.toString))}, text)

    // create an ajax select box
    def doSelect(msg: NodeSeq) =
    ajaxSelect((1 to 50).toList.map(i => (i.toString, i.toString)),
               Full(1.toString),
               v => DisplayMessage(msgName,
                                   bind("sel", msg, "number" -> Text(v)),
                                   5 seconds, 1 second))

    // build up an ajax text box
    def doText(msg: NodeSeq) =
    ajaxText("", v => DisplayMessage(msgName,
                                     bind("text", msg, "value" -> Text(v)),
                                     4 seconds, 1 second))



    // bind the view to the functionality
    bind("ajax", xhtml,
         "clicker" -> doClicker _,
         "select" -> doSelect _,
         "text" -> doText _,
         "auto" -> AutoComplete("", buildQuery _, _ => ()))
  }

  private def buildQuery(current: String, limit: Int): Seq[String] = {
    logger.info("Checking on server side with "+current+" limit "+limit)
    (1 to limit).map(n => current+""+n)
  }

  def time = Text(timeNow.toString)

  def buttonClick = {
    import js.JE._

    "* [onclick]" #> SHtml.ajaxCall(ValById("the_input"),
				    s => SetHtml("messages",
						 <i>Text box is {s}</i>))
  }
}
}
}
}
