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

import scala.xml.NodeSeq

import common.{Box, Empty, Failure, Full}
import http.js.JE.{JsNull, JsObj, JsRaw, Str}
import http.S
import json.JsonAST._
import json.Printer
import net.liftweb.record.{Field, FieldHelpers, MandatoryTypedField, Record}
import util.Helpers._

import org.bson.types.ObjectId
import com.mongodb.DBRef

/*
* Field for storing an ObjectId
*/
class ObjectIdField[OwnerType <: MongoRecord[OwnerType]](rec: OwnerType)
  extends Field[ObjectId, OwnerType]
  with MandatoryTypedField[ObjectId]
{

  def owner = rec

  def defaultValue = ObjectId.get

  def setFromAny(in: Any): Box[ObjectId] = genericSetFromAny(in)

  def setFromJValue(jvalue: JValue): Box[ObjectId] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JObject(JField("$oid", JString(s)) :: Nil) => setFromString(s)
    case JString(s) => setFromString(s)
    case other => setBox(FieldHelpers.expectedA("JObject", other))
  }

  def setFromString(in: String): Box[ObjectId] =
    if (ObjectId.isValid(in))
      setBox(Full(new ObjectId(in)))
    else
      setBox(Failure("Invalid ObjectId string: "+in))

  private def elem =
    S.fmapFunc(S.SFuncHolder(this.setFromString(_))){funcName =>
      <input type="text"
        name={funcName}
        value={valueBox.map(s => s.toString) openOr ""}
        tabindex={tabIndex toString}/>
    }

  def toForm =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> (id + "_field")))
      case _ => Full(elem)
    }

  def asJs = valueBox.map { v =>
    if (Meta.Reflection.isObjectIdSerializerUsed(owner.meta.formats))
      JsObj(("$oid", Str(v.toString)))
    else
      Str(v.toString)
  } openOr JsNull
  /*
  def asJs = asJValue match {
    case JNothing => JsNull
    case jv => JsRaw(Printer.compact(render(jv)))
  }
  */

  def asJValue: JValue = valueBox.map(v => Meta.Reflection.objectIdAsJValue(v)(owner.meta.formats)) openOr (JNothing: JValue)
}

}
}
}
}
