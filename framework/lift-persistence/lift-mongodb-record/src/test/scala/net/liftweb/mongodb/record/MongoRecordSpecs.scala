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

import common._
import http.js.JE._
import http.js.JsExp
import json.JsonAST._
import json.Printer

import java.util.{Date, UUID}
import java.util.regex.Pattern

import org.bson.types.ObjectId
import org.specs.Specification
import org.specs.runner.JUnit4

import net.liftweb.record.field.Countries

import com.mongodb.DBRef

class MongoRecordSpecsTest extends JUnit4(MongoRecordSpecs)

object MongoRecordSpecs extends Specification with MongoTestKit {

  import fixtures._

  "MongoRecord field introspection" should {
    checkMongoIsRunning

    val rec = MongoFieldTypeTestRecord.createRecord
    val allExpectedFieldNames: List[String] = "id" :: (for {
      typeName <- "Date JsonObject ObjectId Pattern UUID".split(" ")
      flavor <- "mandatory legacyOptional".split(" ")
    } yield flavor + typeName + "Field").toList

    "introspect only the expected fields" in {
      rec.fields().map(_.name).sort(_ < _) must_== allExpectedFieldNames.sort(_ < _)
    }

    "correctly look up fields by name" in {
      for (name <- allExpectedFieldNames) {
        rec.fieldByName(name) must verify(_.isDefined)
      }
    }

    "not look up fields by bogus names" in {
      for (name <- allExpectedFieldNames) {
        rec.fieldByName("x" + name + "y") must not(verify(_.isDefined))
      }
    }
  }

  "MongoRecord lifecycle callbacks" should {
    checkMongoIsRunning

    def testOneHarness(scope: String, f: LifecycleTestRecord => HarnessedLifecycleCallbacks): Unit = {
      ("be called before validation when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeValidationHarness = () => triggered = true
        rec.foreachCallback(_.beforeValidation)
        triggered must_== true
      }

      ("be called after validation when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterValidationHarness = () => triggered = true
        rec.foreachCallback(_.afterValidation)
        triggered must_== true
      }

      ("be called around validate when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggeredBefore = false
        var triggeredAfter = false
        f(rec).beforeValidationHarness = () => triggeredBefore = true
        f(rec).afterValidationHarness = () => triggeredAfter = true
        rec.validate must_== Nil
        triggeredBefore must_== true
        triggeredAfter must_== true
      }

      ("be called before save when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeSaveHarness = () => triggered = true
        rec.foreachCallback(_.beforeSave)
        triggered must_== true
      }

      ("be called before create when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeCreateHarness = () => triggered = true
        rec.foreachCallback(_.beforeCreate)
        triggered must_== true
      }

      ("be called before update when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeUpdateHarness = () => triggered = true
        rec.foreachCallback(_.beforeUpdate)
        triggered must_== true
      }

      ("be called after save when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterSaveHarness = () => triggered = true
        rec.foreachCallback(_.afterSave)
        triggered must_== true
      }

      ("be called after create when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterCreateHarness = () => triggered = true
        rec.foreachCallback(_.afterCreate)
        triggered must_== true
      }

      ("be called after update when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterUpdateHarness = () => triggered = true
        rec.foreachCallback(_.afterUpdate)
        triggered must_== true
      }

      ("be called before delete when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeDeleteHarness = () => triggered = true
        rec.foreachCallback(_.beforeDelete)
        triggered must_== true
      }

      ("be called after delete when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterDeleteHarness = () => triggered = true
        rec.foreachCallback(_.afterDelete)
        triggered must_== true
      }
    }

    testOneHarness("the record level", rec => rec)
    testOneHarness("the inner object level", rec => rec.innerObjectWithCallbacks: HarnessedLifecycleCallbacks)
    testOneHarness("the field level", rec => rec.stringFieldWithCallbacks: HarnessedLifecycleCallbacks)
  }

  "MongoRecord" should {
    checkMongoIsRunning

    val binData: Array[Byte] = Array(18, 19, 20)
    val sr1 = SubRecord.createRecord
      .name("SubRecord1")
    val sr2 = SubRecord.createRecord
      .name("SubRecord2")

    val fttr = FieldTypeTestRecord.createRecord
      .mandatoryBooleanField(false)
      .mandatoryCountryField(Countries.USA)
      .mandatoryDecimalField(BigDecimal("3.14"))
      .mandatoryDoubleField(1999)
      .mandatoryEmailField("test@liftweb.net")
      .mandatoryEnumField(MyTestEnum.ONE)
      .mandatoryIntField(99)
      .mandatoryLocaleField("en_US")
      .mandatoryLongField(100L)
      .mandatoryPostalCodeField("55401")
      .mandatoryStringField("string")
      .mandatoryTextareaField("string")
      .mandatoryTimeZoneField("America/Chicago")

    val bftr = BinaryFieldTestRecord.createRecord
      .mandatoryBinaryField(binData)

    val ltr = ListTestRecord.createRecord
      .mandatoryStringListField(List("abc", "def", "ghi"))
      .mandatoryIntListField(List(4, 5, 6))
      .mandatoryMongoJsonObjectListField(List(TypeTestJsonObject(1, "jsonobj1"), TypeTestJsonObject(2, "jsonobj2")))
      .mandatoryMongoSubRecordListField(List(sr1, sr2))

    val mfttr = MongoFieldTypeTestRecord.createRecord
      .mandatoryDateField(new Date)
      .mandatoryJsonObjectField(TypeTestJsonObject(1, "jsonobj1"))
      .mandatoryObjectIdField(fttr.id.value)
      .mandatoryPatternField(Pattern.compile("^Mo", Pattern.CASE_INSENSITIVE))
      .mandatoryUUIDField(ltr.id.value)
      
     /* This causes problems if MongoDB is not running 
    if (isMongoRunning) {
      mfttr.mandatoryDBRefField(DBRefTestRecord.createRecord.getRef)
    }*/

    val mtr = MapTestRecord.createRecord
      .mandatoryStringMapField(Map("a" -> "abc", "b" -> "def", "c" -> "ghi"))
      .mandatoryIntMapField(Map("a" -> 4, "b" -> 5, "c" -> 6))

    val srtr = SubRecordTestRecord.createRecord
      .mandatoryMongoSubRecordField(sr1)
      .mandatoryFieldTypeMongoSubRecordField(fttr)
      .mandatoryMongoFieldTypeMongoSubRecordField(mfttr)

    val ref = RefRecord.createRecord.getRef
    val dbrtr = DBRefTestRecord.createRecord
      .mandatoryDBRefField(ref)

    val fttrJson = JObject(List(
      JField("id", JObject(List(JField("$oid", JString(fttr.id.toString))))),
      JField("mandatoryBooleanField", JBool(fttr.mandatoryBooleanField.value)),
      JField("legacyOptionalBooleanField", JNothing),
      JField("optionalBooleanField", JNothing),
      JField("mandatoryCountryField", JInt(fttr.mandatoryCountryField.value.id)),
      JField("legacyOptionalCountryField", JNothing),
      JField("optionalCountryField", JNothing),
      JField("mandatoryDecimalField", JString(fttr.mandatoryDecimalField.value.toString)),
      JField("legacyOptionalDecimalField", JNothing),
      JField("optionalDecimalField", JNothing),
      JField("mandatoryDoubleField", JDouble(fttr.mandatoryDoubleField.value)),
      JField("legacyOptionalDoubleField", JNothing),
      JField("optionalDoubleField", JNothing),
      JField("mandatoryEmailField", JString(fttr.mandatoryEmailField.value)),
      JField("legacyOptionalEmailField", JNothing),
      JField("optionalEmailField", JNothing),
      JField("mandatoryEnumField", JInt(fttr.mandatoryEnumField.value.id)),
      JField("legacyOptionalEnumField", JNothing),
      JField("optionalEnumField", JNothing),
      JField("mandatoryIntField", JInt(fttr.mandatoryIntField.value)),
      JField("legacyOptionalIntField", JNothing),
      JField("optionalIntField", JNothing),
      JField("mandatoryLocaleField", JString(fttr.mandatoryLocaleField.value)),
      JField("legacyOptionalLocaleField", JNothing),
      JField("optionalLocaleField", JNothing),
      JField("mandatoryLongField", JInt(fttr.mandatoryLongField.value)),
      JField("legacyOptionalLongField", JNothing),
      JField("optionalLongField", JNothing),
      JField("mandatoryPostalCodeField", JString(fttr.mandatoryPostalCodeField.value)),
      JField("legacyOptionalPostalCodeField", JNothing),
      JField("optionalPostalCodeField", JNothing),
      JField("mandatoryStringField", JString(fttr.mandatoryStringField.value)),
      JField("legacyOptionalStringField", JNothing),
      JField("optionalStringField", JNothing),
      JField("mandatoryTextareaField", JString(fttr.mandatoryTextareaField.value)),
      JField("legacyOptionalTextareaField", JNothing),
      JField("optionalTextareaField", JNothing),
      JField("mandatoryTimeZoneField", JString(fttr.mandatoryTimeZoneField.value)),
      JField("legacyOptionalTimeZoneField", JNothing),
      JField("optionalTimeZoneField", JNothing)
    ))

    val mfttrJson = JObject(List(
      JField("id", JObject(List(JField("$oid", JString(mfttr.id.toString))))),
      JField("mandatoryDateField", JObject(List(JField("$dt", JString(mfttr.meta.formats.dateFormat.format(mfttr.mandatoryDateField.value)))))),
      JField("legacyOptionalDateField", JNothing),
      JField("mandatoryJsonObjectField", JObject(List(JField("intField", JInt(1)), JField("stringField", JString("jsonobj1"))))),
      JField("legacyOptionalJsonObjectField", JObject(List(JField("intField", JInt(0)), JField("stringField", JString(""))))),
      JField("mandatoryObjectIdField", JObject(List(JField("$oid", JString(mfttr.mandatoryObjectIdField.value.toString))))),
      JField("legacyOptionalObjectIdField", JNothing),
      JField("mandatoryPatternField", JObject(List(JField("$regex", JString(mfttr.mandatoryPatternField.value.pattern)), JField("$flags", JInt(mfttr.mandatoryPatternField.value.flags))))),
      JField("legacyOptionalPatternField", JNothing),
      JField("mandatoryUUIDField", JObject(List(JField("$uuid", JString(mfttr.mandatoryUUIDField.value.toString))))),
      JField("legacyOptionalUUIDField", JNothing)
    ))

    val ltrJson = JObject(List(
      JField("id", JObject(List(JField("$uuid", JString(ltr.id.toString))))),
      JField("mandatoryStringListField", JArray(List(JString("abc"), JString("def"), JString("ghi")))),
      JField("legacyOptionalStringListField", JArray(List())),
      JField("mandatoryIntListField", JArray(List(JInt(4), JInt(5), JInt(6)))),
      JField("legacyOptionalIntListField", JArray(List())),
      JField("mandatoryMongoJsonObjectListField", JArray(List(
        JObject(List(JField("intField", JInt(1)), JField("stringField", JString("jsonobj1")))),
        JObject(List(JField("intField", JInt(2)), JField("stringField", JString("jsonobj2"))))
      ))),
      JField("legacyOptionalMongoJsonObjectListField", JArray(List())),
      JField("mandatoryMongoSubRecordListField", JArray(List(
        JObject(List(JField("name", JString("SubRecord1")))),
        JObject(List(JField("name", JString("SubRecord2"))))
      ))),
      JField("legacyOptionalMongoSubRecordListField", JArray(List()))
    ))

    val mtrJson = JObject(List(
      JField("id", JString(mtr.id.toString)),
      JField("mandatoryStringMapField", JObject(List(
        JField("a", JString("abc")),
        JField("b", JString("def")),
        JField("c", JString("ghi"))
      ))),
      JField("legacyOptionalStringMapField", JObject(List())),
      JField("mandatoryIntMapField", JObject(List(
        JField("a", JInt(4)),
        JField("b", JInt(5)),
        JField("c", JInt(6))
      ))),
      JField("legacyOptionalIntMapField", JObject(List()))
    ))

    val srtrJson = JObject(List(
      JField("id", JInt(srtr.id.value)),
      JField("mandatoryMongoSubRecordField", JObject(List(JField("name", JString("SubRecord1"))))),
      JField("legacyOptionalMongoSubRecordField", JNothing),
      JField("mandatoryFieldTypeMongoSubRecordField", fttrJson),
      JField("legacyOptionalFieldTypeMongoSubRecordField", JNothing),
      JField("mandatoryMongoFieldTypeMongoSubRecordField", mfttrJson),
      JField("legacyMongoFieldTypeMongoSubRecordField", JNothing)
    ))

    "save and retrieve 'standard' type fields with Empty optional fields" in {
      checkMongoIsRunning

      fttr.save

      val fttrFromDb = FieldTypeTestRecord.find(fttr.id.value)
      fttrFromDb must notBeEmpty
      fttrFromDb foreach { tr =>
        tr mustEqual fttr
      }
    }

    "save and retrieve 'standard' type fields with Full optional fields" in {
      checkMongoIsRunning

      fttr.save

      val fttrFromDb = FieldTypeTestRecord.find(fttr.id.value)
      fttrFromDb must notBeEmpty
      fttrFromDb foreach { tr =>
        tr mustEqual fttr
      }
    }

    "delete 'standard' type fields" in {
      checkMongoIsRunning

      fttr.save
      FieldTypeTestRecord.find(fttr.id.value) must notBeEmpty
      fttr.delete_!
      FieldTypeTestRecord.find(fttr.id.value) must beEmpty
    }

    "save and retrieve Mongo type fields" in {
      checkMongoIsRunning

      mfttr.save

      val mfttrFromDb = MongoFieldTypeTestRecord.find(mfttr.id.value)
      mfttrFromDb must notBeEmpty
      mfttrFromDb foreach { tr =>
        tr mustEqual mfttr
      }

      bftr.save

      val bftrFromDb = BinaryFieldTestRecord.find(bftr.id.value)
      bftrFromDb must notBeEmpty
      bftrFromDb foreach { tr =>
        tr mustEqual bftr
      }

      ltr.save

      val ltrFromDb = ListTestRecord.find(ltr.id.value)
      ltrFromDb must notBeEmpty
      ltrFromDb foreach { tr =>
        tr mustEqual ltr
      }

      mtr.save

      val mtrFromDb = MapTestRecord.find(mtr.id.value)
      mtrFromDb must notBeEmpty
      mtrFromDb foreach { tr =>
        tr mustEqual mtr
      }

      srtr.save

      val srtrFromDb = SubRecordTestRecord.find(srtr.id.value)
      srtrFromDb must notBeEmpty
      srtrFromDb foreach { tr =>
        tr mustEqual srtr
      }

      dbrtr.save

      val dbrtrFromDb = DBRefTestRecord.find(dbrtr.id)
      dbrtrFromDb must notBeEmpty
      dbrtrFromDb foreach { tr =>
        tr mustEqual dbrtr
      }
    }

    "delete Mongo type fields" in {
      checkMongoIsRunning

      mfttr.save
      MongoFieldTypeTestRecord.find(mfttr.id.value) must notBeEmpty
      mfttr.delete_!
      MongoFieldTypeTestRecord.find(mfttr.id.value) must beEmpty

      bftr.save
      BinaryFieldTestRecord.find(bftr.id.value) must notBeEmpty
      bftr.delete_!
      BinaryFieldTestRecord.find(bftr.id.value) must beEmpty

      ltr.save
      ListTestRecord.find(ltr.id.value) must notBeEmpty
      ltr.delete_!
      ListTestRecord.find(ltr.id.value) must beEmpty

      mtr.save
      MapTestRecord.find(mtr.id.value) must notBeEmpty
      mtr.delete_!
      MapTestRecord.find(mtr.id.value) must beEmpty

      srtr.save
      SubRecordTestRecord.find(srtr.id.value) must notBeEmpty
      srtr.delete_!
      SubRecordTestRecord.find(srtr.id.value) must beEmpty

      dbrtr.save
      DBRefTestRecord.find(dbrtr.id) must notBeEmpty
      dbrtr.delete_!
      DBRefTestRecord.find(dbrtr.id) must beEmpty
    }

    "convert 'standard' type fields to JValue" in {
      fttr.asJValue mustEqual fttrJson
    }

    "convert Mongo type fields to JValue" in {
      checkMongoIsRunning

      mfttr.asJValue mustEqual mfttrJson

      ltr.asJValue mustEqual ltrJson

      mtr.asJValue mustEqual mtrJson

      val srtrAsJValue = srtr.asJValue
      srtrAsJValue \\ "id" mustEqual srtrJson \\ "id"
      srtrAsJValue \\ "mandatoryMongoSubRecordField" mustEqual srtrJson \\ "mandatoryMongoSubRecordField"
      srtrAsJValue \\ "legacyOptionalMongoSubRecordField" mustEqual srtrJson \\ "legacyOptionalMongoSubRecordField"
      srtrAsJValue \\ "mandatoryFieldTypeMongoSubRecordField" mustEqual srtrJson \\ "mandatoryFieldTypeMongoSubRecordField"
      srtrAsJValue \\ "legacyOptionalFieldTypeMongoSubRecordField" mustEqual srtrJson \\ "legacyOptionalFieldTypeMongoSubRecordField"
      srtrAsJValue \\ "mandatoryMongoFieldTypeMongoSubRecordField" mustEqual srtrJson \\ "mandatoryMongoFieldTypeMongoSubRecordField"
      srtrAsJValue \\ "legacyMongoFieldTypeMongoSubRecordField" mustEqual srtrJson \\ "legacyMongoFieldTypeMongoSubRecordField"
      //srtr.asJValue mustEqual srtrJson // The fields don't seem to be in the same order, but they are equal when compared individually.

      dbrtr.asJValue mustEqual JObject(List(
        JField("_id", JString(dbrtr.id.toString)),
        JField("mandatoryDBRefField", JNothing),
        JField("legacyOptionalDBRefField", JNothing)
      ))
    }

    "convert Mongo type fields to JsExp" in {
      checkMongoIsRunning

      /*
      mfttr.asJsExp mustEqual JsObj(
        ("id", JsObj(("$oid", Str(mfttr.id.toString)))),
        ("mandatoryDateField", JsObj(("$dt", Str(mfttr.meta.formats.dateFormat.format(mfttr.mandatoryDateField.value))))),
        ("legacyOptionalDateField", Str("null")),
        ("mandatoryJsonObjectField", JsObj(("intField", Num(1)), ("stringField", Str("jsonobj1")))),
        ("legacyOptionalJsonObjectField", JsObj(("intField", Num(0)), ("stringField", Str("")))),
        ("mandatoryObjectIdField", JsObj(("$oid", Str(mfttr.mandatoryObjectIdField.value.toString)))),
        ("legacyOptionalObjectIdField", Str("null")),
        ("mandatoryPatternField", JsObj(("$regex", Str(mfttr.mandatoryPatternField.value.pattern)), ("$flags", Num(mfttr.mandatoryPatternField.value.flags)))),
        ("legacyOptionalPatternField", Str("null")),
        ("mandatoryUUIDField", JsObj(("$uuid", Str(mfttr.mandatoryUUIDField.value.toString)))),
        ("legacyOptionalUUIDField", Str("null"))
      )*/

      /*
      ltr.asJsExp mustEqual JsObj(
        ("id", JsObj(("$oid", Str(ltr.id.toString)))),
        ("mandatoryStringListField", JsArray(Str("abc"), Str("def"), Str("ghi"))),
        ("legacyOptionalStringListField", JsArray()),
        ("mandatoryIntListField", JsArray(Num(4), Num(5), Num(6))),
        ("legacyOptionalIntListField", JsArray()),
        ("mandatoryMongoJsonObjectListField", JsArray(
          JsObj(("intField", Num(1)), ("stringField", Str("jsonobj1"))),
          JsObj(("intField", Num(2)), ("stringField", Str("jsonobj2")))
        )),
        ("legacyOptionalMongoJsonObjectListField", JsArray()),
        ("mandatoryMongoSubRecordListField", JsArray(
          JsObj(("name", Str("SubRecord1"))),
          JsObj(("name", Str("SubRecord2")))
        ))
      )

      mtr.asJsExp mustEqual JsObj(
        ("id", JsObj(("$oid", Str(mtr.id.toString)))),
        ("mandatoryStringMapField", JsObj(
          ("a", Str("abc")),
          ("b", Str("def")),
          ("c", Str("ghi"))
        )),
        ("legacyOptionalStringMapField", JsObj()),
        ("mandatoryIntMapField", JsObj(
          ("a", Num(4)),
          ("b", Num(5)),
          ("c", Num(6))
        )),
        ("legacyOptionalIntMapField", JsObj())
      )

      srtr.asJsExp mustEqual new JsExp {
        lazy val toJsCmd = Printer.compact(render(srtrJson))
      }

      dbrtr.asJsExp mustEqual JsObj(
        ("_id", Str(dbrtr.id.toString)),
        ("mandatoryDBRefField", Str("null")),
        ("legacyOptionalDBRefField", Str("null"))
      )*/
    }

    "get set from json string using lift-json parser" in {
      checkMongoIsRunning
      val mfftrFromJson = MongoFieldTypeTestRecord.fromJsonString(Printer.compact(render(mfttrJson)))
      mfftrFromJson must notBeEmpty
      mfftrFromJson foreach { tr =>
        tr mustEqual mfttr
      }

      val ltrFromJson = ListTestRecord.fromJsonString(Printer.compact(render(ltrJson)))
      ltrFromJson must notBeEmpty
      ltrFromJson foreach { tr =>
        tr mustEqual ltr
      }

      val mtrFromJson = MapTestRecord.fromJsonString(Printer.compact(render(mtrJson)))
      mtrFromJson must notBeEmpty
      mtrFromJson foreach { tr =>
        tr mustEqual mtr
      }
    }

    /*
    "retrieve MongoRef objects properly" in {
      fttr.save
      ltr.save
      mfttr.save

      mfttr.mandatoryObjectIdField.obj mustEqual Full(fttr)
      mfttr.mandatoryUUIDField.obj mustEqual Full(ltr)
    }
    */

    "handle null properly" in {
      checkMongoIsRunning

      val ntr = NullTestRecord.createRecord
      ntr.nullstring.set(null)
      ntr.jsonobjlist.set(List(JsonObj("1", null), JsonObj("2", "jsonobj2")))

      ntr.save must_== ntr

      val ntrFromDb = NullTestRecord.find(ntr.id.value)

      ntrFromDb must notBeEmpty

      ntrFromDb foreach { n =>
        // goes in as
        ntr.nullstring.valueBox.map(_ must beNull)
        ntr.nullstring.value must beNull
        // comes out as
        n.nullstring.valueBox.map(_ must_== "")
        n.nullstring.value must_== ""
        // JsonObjects
        n.jsonobjlist.value.size must_== 2
        ntr.jsonobjlist.value.size must_== 2
        n.jsonobjlist.value(0).id must_== ntr.jsonobjlist.value(0).id
        n.jsonobjlist.value(0).name must beNull
        ntr.jsonobjlist.value(0).name must beNull
        n.jsonobjlist.value(1).id must_== ntr.jsonobjlist.value(1).id
        n.jsonobjlist.value(1).name must_== ntr.jsonobjlist.value(1).name
      }
    }

    "handle Box using JsonBoxSerializer properly" in {
      checkMongoIsRunning
      
      val btr = BoxTestRecord.createRecord
      btr.jsonobjlist.set(
        BoxTestJsonObj("1", Empty, Full("Full String1"), Failure("Failure1")) ::
        BoxTestJsonObj("2", Empty, Full("Full String2"), Failure("Failure2")) ::
        Nil
      )

      btr.save

      val btrFromDb = BoxTestRecord.find(btr.id.value)

      btrFromDb must notBeEmpty

      btrFromDb foreach { b =>
        b.jsonobjlist.value.size must_== 2
        btr.jsonobjlist.value.size must_== 2
        val sortedList = b.jsonobjlist.value.sort(_.id < _.id)
        sortedList(0).boxEmpty must_== Empty
        sortedList(0).boxFull must_== Full("Full String1")
        sortedList(0).boxFail must_== Failure("Failure1")
      }
    }

  }
}

}
}
}
