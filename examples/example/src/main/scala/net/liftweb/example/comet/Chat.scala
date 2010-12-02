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

import _root_.net.liftweb._
import http._
import common._
import actor._
import util._
import Helpers._
import _root_.scala.xml._
import S._
import SHtml._
import js._
import JsCmds._
import JE._
import net.liftweb.http.js.jquery.JqJsCmds.{AppendHtml}

class Chat extends CometActor with CometListener {
  private var userName = ""
  private var chats: List[ChatLine] = Nil

  /* need these vals to be set eagerly, within the scope
   * of Comet component constructor
   */
  private val ulId = S.attr("ul_id") openOr "some_ul_id"

  private val liId = S.attr("li_id")

  private lazy val li = liId.
  flatMap{ Helpers.findId(defaultXml, _) } openOr NodeSeq.Empty

  private val inputId = Helpers.nextFuncName

  // handle an update to the chat lists
  // by diffing the lists and then sending a partial update
  // to the browser
  override def lowPriority = {
    case ChatServerUpdate(value) => {
      val update = (value -- chats).reverse.
      map(b => AppendHtml(ulId, line(b)))

      partialUpdate(update)
      chats = value
    }
  }

  // render the input area by binding the
  // appropriate dynamically generated code to the
  // view supplied by the template
  override lazy val fixedRender: Box[NodeSeq] =
    S.runTemplate("_chat_fixed" :: Nil,
                  "postit" -> Helpers.evalElemWithId {
                    (id, elem) => 
                      SHtml.onSubmit((s: String) => {
                        ChatServer ! ChatServerMsg(userName, s.trim)
                        SetValById(id, "")
                      })(elem)
                  } _)

  // display a line
  private def line(c: ChatLine) = {
    ("name=when" #> hourFormat(c.when) &
     "name=who" #> c.user &
     "name=body" #> c.msg)(li)
  }

  // display a list of chats
  private def displayList: NodeSeq = chats.reverse.flatMap(line)

  // render the whole list of chats
  override def render = {
    "name=chat_name" #> userName &
    ("#"+ulId+" *") #> displayList
  }

  // setup the component
  override def localSetup {
    askForName
    super.localSetup
  }

  // register as a listener
  def registerWith = ChatServer

  // ask for the user's name
  private def askForName {
    if (userName.length == 0) {
      ask(new AskName, "what's your username") {
        case s: String if (s.trim.length > 2) =>
          userName = s.trim
          reRender(true)

        case _ =>
          askForName
          reRender(false)
      }
    }
  }

}
}
}
}
