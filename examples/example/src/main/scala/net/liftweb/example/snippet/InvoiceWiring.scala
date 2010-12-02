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

case class Line(guid: String, name: String, price: Double, taxable: Boolean)

/**
 * An invoice system with subtotals, tax, etc.
 */
class InvoiceWiring {
  private object Info {
    val invoices = ValueCell(List(newLine))
    val taxRate = ValueCell(0.05d)
    val subtotal = invoices.lift(_.foldLeft(0d)(_ + _.price))
    val taxable = invoices.lift(_.filter(_.taxable).
                                foldLeft(0D)(_ + _.price))

    val tax = taxRate.lift(taxable) {_ * _}

    val total = subtotal.lift(tax) {_ + _}    
  }

  def subtotal(in: NodeSeq) = WiringUI.asText(in, Info.subtotal)

  def taxable(in: NodeSeq) = WiringUI.asText(in, Info.taxable)

  def tax(in: NodeSeq) = WiringUI.asText(in, Info.tax, JqWiringSupport.fade)

  def total(in: NodeSeq) = WiringUI.asText(in, Info.total, JqWiringSupport.fade)

  def taxRate = SHtml.ajaxText(Info.taxRate.get.toString,
                               s => {
                                 Helpers.asDouble(s).foreach {
                                   Info.taxRate.set
                                 }
                                 Noop
                               })

  def showLines = "* *" #> (Info.invoices.get.flatMap(renderLine): NodeSeq)

  def addLine(ns: NodeSeq): NodeSeq = {
    val div = S.attr("div") openOr "where"
    SHtml.ajaxButton(ns, () => {
      val theLine = appendLine
      val guid = theLine.guid
      JqJsCmds.AppendHtml(div, renderLine(theLine))
    })
  }

  private def renderLine(theLine: Line): NodeSeq =
    <div id={theLine.guid}>{
      SHtml.ajaxText(theLine.name,
                     s => {
                       mutateLine(theLine.guid) {
                         l => Line(l.guid, s, l.price, l.taxable)
                       }
                       Noop
                     })
    }
  
  {
    SHtml.ajaxText(theLine.price.toString,
                   s => {
                     Helpers.asDouble(s).foreach {
                       d => 
                         mutateLine(theLine.guid) {
                           l => Line(l.guid, l.name, d, l.taxable)
                         }
                     }
                     Noop
                   })
  }

  {
    SHtml.ajaxCheckbox(theLine.taxable,
                      b => {
                        mutateLine(theLine.guid) {
                          l => Line(l.guid, l.name, l.price, b)
                        }
                        Noop
                      })
  }
  </div>

    private def newLine = Line(nextFuncName, "", 0, false)
    
    private def appendLine: Line = {
      val ret = newLine
      Info.invoices.set(ret :: Info.invoices.get)
      ret
    }
    
    private def mutateLine(guid: String)(f: Line => Line) {
      val all = Info.invoices.get
      val head = all.filter(_.guid == guid).map(f)
      val rest = all.filter(_.guid != guid)
      Info.invoices.set(head ::: rest)
    }


}

}
}
}
