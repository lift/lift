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
package record {
package field {

import common._
import http.SHtml
import util.Helpers._
import scala.xml.NodeSeq

trait ChoiceTypedField extends TypedField[String] {
  /** Options for select list **/
  def options: List[(Box[String], String)]

  /** Label for the selection item representing Empty, show when this field is optional. Defaults to the empty string. */
  def emptyOptionLabel: String = ""

  def buildDisplayList: List[(Box[String], String)]

  private def elem = SHtml.selectObj[Box[String]](
    buildDisplayList,
    Full(valueBox),
    setBox(_)
  ) % ("tabindex" -> tabIndex.toString)

  override def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> (id + "_field")))
      case _ => Full(elem)
    }
}

abstract class ChoiceField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int)
  extends StringField(rec, maxLength) with ChoiceTypedField {

  def buildDisplayList: List[(Box[String], String)] = {
    if (optional_?) (Empty, emptyOptionLabel)::options else options
  }
}

abstract class OptionalChoiceField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int)
  extends OptionalStringField(rec, maxLength) with ChoiceTypedField {

  def buildDisplayList: List[(Box[String], String)] = (Empty, emptyOptionLabel)::options
}

}
}
}
