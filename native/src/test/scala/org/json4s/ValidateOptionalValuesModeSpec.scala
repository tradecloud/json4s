package org.json4s

import org.scalatest.wordspec.AnyWordSpec
import org.json4s.native.Document

//TODO: update
class NativeValidateOptionalValuesModeSpec
  extends StrictOptionParsingModeSpec[Document]("Native")
  with native.JsonMethods

abstract class ValidateOptionalValuesModeSpec[T](mod: String) extends AnyWordSpec with JsonMethods[T] {

  // TODO: move this into NativeStrictOptionParsingModeSpec
  implicit lazy val formats: Formats = new DefaultFormats { override val validateOptionalValues = true }

  val doubleForIntJson =
    """{ "someDouble": 10.0, "someString": "abc", "someInt": 10.0, "someMap": {}, "someBoolean": true }"""
  val booleanForIntJson =
    """{ "someDouble": 10.0, "someString": "abc", "someInt": true, "someMap": {}, "someBoolean": true }"""
  val stringForIntJson =
    """{ "someDouble": 10.0, "someString": "abc", "someInt": "10", "someMap": {}, "someBoolean": true }"""
  val mapForIntJson =
    """{ "someDouble": 10.0, "someString": "abc", "someInt": {}, "someMap": {}, "someBoolean": true }"""

  val intForDoubleJson =
    """{ "someDouble": 10, "someString": "abc", "someInt": 10, "someMap": {}, "someBoolean": true }"""
  val booleanForDoubleJson =
    """{ "someDouble": true, "someString": "abc", "someInt": 10, "someMap": {}, "someBoolean": true }"""
  val stringForDoubleJson =
    """{ "someDouble": "10.0", "someString": "abc", "someInt": 10, "someMap": {}, "someBoolean": true }"""
  val mapForDoubleJson =
    """{ "someDouble": {}, "someString": "abc", "someInt": 10, "someMap": {}, "someBoolean": true }"""

  val intForBooleanJson =
    """{ "someDouble": 10.0, "someString": "abc", "someInt": 10, "someMap": {}, "someBoolean": 10 }"""
  val doubleForBooleanJson =
    """{ "someDouble": 10.0, "someString": "abc", "someInt": 10, "someMap": {}, "someBoolean": 10.0 }"""
  val stringForBooleanJson =
    """{ "someDouble": 10.0, "someString": "abc", "someInt": 10, "someMap": {}, "someBoolean": "true" }"""
  val mapForBooleanJson =
    """{ "someDouble": 10.0, "someString": "abc", "someInt": 10, "someMap": {}, "someBoolean": {} }"""

  val doubleForStringJson =
    """{ "someDouble": 10.0, "someString": 10.0, "someInt": 10, "someMap": {}, "someBoolean": true }"""
  val intForStringJson =
    """{ "someDouble": 10.0, "someString": 10, "someInt": 10, "someMap": {}, "someBoolean": true }"""
  val booleanForStringJson =
    """{ "someDouble": 10.0, "someString": false, "someInt": 10, "someMap": {}, "someBoolean": true }"""
  val mapForStringJson =
    """{ "someDouble": 10.0, "someString": {}, "someInt": 10, "someMap": {}, "someBoolean": true }"""

  val intForMapJson = """{ "someDouble": 10.0, "someString": {}, "someInt": 10, "someMap": 10, "someBoolean": true }"""
  val doubleForMapJson =
    """{ "someDouble": 10.0, "someString": {}, "someInt": 10, "someMap": 10.0, "someBoolean": true }"""
  val booleanForMapJson =
    """{ "someDouble": 10.0, "someString": {}, "someInt": 10, "someMap": true, "someBoolean": true }"""
  val stringForMapJson =
    """{ "someDouble": 10.0, "someString": {}, "someInt": 10, "someMap": "some string", "someBoolean": true }"""

  val correctJson =
    """{ "someDouble": 10.0, "someString": "someString", "someInt": 10, "someMap": {}, "someBoolean": true }"""

  (mod + " case class with optional values in strict mode") should {
    "throw an error on parsing a string for an int" in {
      assertThrows[MappingException] { parse(stringForIntJson).extract[ValidatedOptionalValueModel] }
    }
    "throw an error on parsing a boolean for an int" in {
      assertThrows[MappingException] { parse(booleanForIntJson).extract[ValidatedOptionalValueModel] }
    }
    "throw an error on parsing a map for an int" in {
      assertThrows[MappingException] { parse(mapForIntJson).extract[ValidatedOptionalValueModel] }
    }
    "parse double as an int" in {
      val model = parse(doubleForIntJson).extract[ValidatedOptionalValueModel]
      assert(model.someInt == Some(10))
    }

    "throw an error on parsing a string for a double" in {
      assertThrows[MappingException] { parse(stringForDoubleJson).extract[ValidatedOptionalValueModel] }
    }
    "throw an error on parsing a boolean for a double" in {
      assertThrows[MappingException] { parse(booleanForDoubleJson).extract[ValidatedOptionalValueModel] }
    }
    "throw an error on parsing a map for a double" in {
      assertThrows[MappingException] { parse(mapForDoubleJson).extract[ValidatedOptionalValueModel] }
    }
    "parse int as a double" in {
      val model = parse(intForDoubleJson).extract[ValidatedOptionalValueModel]
      assert(model.someInt == Some(10.0))
    }

    "throw an error on parsing a int for a boolean" in {
      assertThrows[MappingException] { parse(intForBooleanJson).extract[ValidatedOptionalValueModel] }
    }
    "throw an error on parsing a double for a boolean" in {
      assertThrows[MappingException] { parse(doubleForBooleanJson).extract[ValidatedOptionalValueModel] }
    }
    "throw an error on parsing a string for a boolean" in {
      assertThrows[MappingException] { parse(stringForBooleanJson).extract[ValidatedOptionalValueModel] }
    }
    "throw an error on parsing a map for a boolean" in {
      assertThrows[MappingException] { parse(mapForBooleanJson).extract[ValidatedOptionalValueModel] }
    }

    "throw an error on parsing a boolean for a string" in {
      assertThrows[MappingException] { parse(booleanForStringJson).extract[ValidatedOptionalValueModel] }
    }
    "throw an error on parsing a map for a string" in {
      assertThrows[MappingException] { parse(mapForStringJson).extract[ValidatedOptionalValueModel] }
    }
    "parse int as a string" in {
      val model = parse(intForStringJson).extract[ValidatedOptionalValueModel]
      assert(model.someString == Some("10"))
    }
    "parse double as a string" in {
      val model = parse(doubleForStringJson).extract[ValidatedOptionalValueModel]
      assert(model.someString == Some("10.0"))
    }

    "throw an error on parsing a int for a map" in {
      assertThrows[MappingException] { parse(intForMapJson).extract[ValidatedOptionalValueModel] }
    }
    "throw an error on parsing a double for a map" in {
      assertThrows[MappingException] { parse(doubleForMapJson).extract[ValidatedOptionalValueModel] }
    }
    "throw an error on parsing a string for a map" in {
      assertThrows[MappingException] { parse(stringForMapJson).extract[ValidatedOptionalValueModel] }
    }
    "throw an error on parsing a boolean for a map" in {
      assertThrows[MappingException] { parse(booleanForMapJson).extract[ValidatedOptionalValueModel] }
    }

    "extract the class if all values are correctly typed" in {
      val model = parse(correctJson).extract[ValidatedOptionalValueModel]
      assert(model.someDouble == Some(10.0))
      assert(model.someInt == Some(10))
      assert(model.someString == Some("someString"))
      assert(model.someMap == Some(Map[String, Any]()))
      assert(model.someBoolean == Some(true))
    }
  }
}

case class ValidatedOptionalValueModel(
  someInt: Option[Int],
  someDouble: Option[Double],
  someString: Option[String],
  someMap: Option[Map[String, Any]],
  someBoolean: Option[Boolean]
)
