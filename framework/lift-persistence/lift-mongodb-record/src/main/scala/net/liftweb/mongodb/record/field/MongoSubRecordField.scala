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
package mongodb {
package record {
package field {

import common._
import http.js.JsExp
import http.js.JE.JsNull
import json.JsonAST._
import json.Printer

import net.liftweb.record._
import com.mongodb.DBObject

import scala.xml._

/** Field that contains an entire record represented as an inline object value. Inspired by JSONRecord */
class MongoSubRecordField[OwnerType <: MongoRecord[OwnerType], SubRecordType <: MongoRecord[SubRecordType]]
                        (rec: OwnerType, valueMeta: MongoMetaRecord[SubRecordType])(implicit subRecordType: Manifest[SubRecordType])
  extends Field[SubRecordType, OwnerType]
  with MandatoryTypedField[SubRecordType]
{
  def this(rec: OwnerType, valueMeta: MongoMetaRecord[SubRecordType], value: SubRecordType)
          (implicit subRecordType: Manifest[SubRecordType]) = {
      this(rec, value.meta)
      set(value)
  }

  def this(rec: OwnerType, valueMeta: MongoMetaRecord[SubRecordType], value: Box[SubRecordType])
          (implicit subRecordType: Manifest[SubRecordType]) = {
      this(rec, valueMeta)
      setBox(value)
  }

  def owner = rec
  def asJs = asJValue match {
    case JNothing => JsNull
    case jv => new JsExp {
      lazy val toJsCmd = Printer.compact(render(jv))
    }
  }
  def toForm: Box[NodeSeq] = Empty
  def defaultValue = valueMeta.createRecord

  def setFromString(s: String): Box[SubRecordType] = valueMeta.fromJsonString(s)

  def setFromAny(in: Any): Box[SubRecordType] = in match {
    case dbo: DBObject => setBox(Full(valueMeta.fromDBObject(dbo)))
    case _ => genericSetFromAny(in)
  }

  def asJValue: JValue = valueBox.map(_.asJValue) openOr (JNothing: JValue)
  def setFromJValue(jvalue: JValue): Box[SubRecordType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case _ => setBox(valueMeta.fromJValue(jvalue))
  }
}
}
}
}
}
