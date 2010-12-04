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
import Helpers._
import js.JsCmds._
import js.jquery._
import _root_.scala.xml.{NodeSeq, Text}

/**
 * A simple example of Wiring.  The count of done
 * To-do items 
 */
class SimpleWiring {
  // define the cells
  private val feedFish = ValueCell(false)
  private val walkDog = ValueCell(false)
  private val doDishes = ValueCell(false)
  private val watchTv = ValueCell(false)

  // Our count is the collection of cells and we sum them up
  private val count = SeqCell(feedFish,
                              walkDog,
                              doDishes,
                              watchTv).lift {
    _.map(_.toInt).reduceLeft(_ + _)}

  private class BtoI(b: Boolean) {def toInt: Int = if (b) 1 else 0}
  private implicit def bToI(b: Boolean): BtoI = new BtoI(b)

  // define the count transformation
  def count(in: NodeSeq): NodeSeq = 
    WiringUI.asText(in, count, JqWiringSupport.fade)

  def toDo = {
    import SHtml._
    "* *" #> List[NodeSeq](
      <span>Feed Fish {ajaxCheckboxElem(feedFish)}</span>,
      <span>Walk Dog {ajaxCheckboxElem(walkDog)}</span>,
      <span>Do Dishes {ajaxCheckboxElem(doDishes)}</span>,
      <span>Watch TV {ajaxCheckboxElem(watchTv)}</span>)
  }
  
}

}
}
}
