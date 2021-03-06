package com.mpc.scalats.core

import com.mpc.scalats.core.ScalaModel._
import org.scalatest._

import scala.reflect.runtime.universe._

/**
  * Created by Milosz on 06.12.2016.
  */
class ScalaParserSpec extends FlatSpec with Matchers {

  it should "parse case class with one primitive member" in {
    val parsed = ScalaParser.parseCaseClasses(List(TestTypes.TestClass1Type, TestTypes.TestTrait1, TestTypes.TestTrait2))
    val expected = CaseClass("TestClass1", List(CaseClassMember("name", StringRef)), List.empty, Some("Trait1"))
    parsed should contain(expected)
  }

  it should "parse generic case class with one member" in {
    val parsed = ScalaParser.parseCaseClasses(List(TestTypes.TestClass2Type))
    val expected = CaseClass("TestClass2", List(CaseClassMember("name", TypeParamRef("T"))), List("T"))
    parsed should contain(expected)
  }

  it should "parse generic case class with one member list of type parameter" in {
    val parsed = ScalaParser.parseCaseClasses(List(TestTypes.TestClass3Type))
    val expected = CaseClass(
      "TestClass3",
      List(CaseClassMember("name", SeqRef(TypeParamRef("T")))),
      List("T")
    )
    parsed should contain(expected)
  }

  it should "parse generic case class with one optional member" in {
    val parsed = ScalaParser.parseCaseClasses(List(TestTypes.TestClass5Type))
    val expected = CaseClass(
      "TestClass5",
      List(CaseClassMember("name", OptionRef(TypeParamRef("T")))),
      List("T")
    )
    parsed should contain(expected)
  }

  it should "parse empty sealed trait" in {
    val parsed = ScalaParser.parseCaseClasses(List(TestTypes.TestTrait1))
    val expected = CaseClass(
      "Trait1",
      Nil,
      Nil
    )
    parsed should contain(expected)
  }

  it should "correctly detect involved types" in {
    val parsed = ScalaParser.parseCaseClasses(List(TestTypes.TestClass6Type, TestTypes.TestTrait1, TestTypes.TestTrait2))
    parsed should have length 7
  }

}

object TestTypes {

  implicit val mirror = runtimeMirror(getClass.getClassLoader)
  val TestClass1Type = typeFromName("com.mpc.scalats.core.TestTypes.TestClass1")
  val TestClass2Type = typeFromName("com.mpc.scalats.core.TestTypes.TestClass2")
  val TestClass3Type = typeFromName("com.mpc.scalats.core.TestTypes.TestClass3")
  val TestClass4Type = typeFromName("com.mpc.scalats.core.TestTypes.TestClass4")
  val TestClass5Type = typeFromName("com.mpc.scalats.core.TestTypes.TestClass5")
  val TestClass6Type = typeFromName("com.mpc.scalats.core.TestTypes.TestClass6")
  val TestTrait1 = typeFromName("com.mpc.scalats.core.TestTypes.Trait1")
  val TestTrait2 = typeFromName("com.mpc.scalats.core.TestTypes.Trait2")

  private def typeFromName(name: String) = mirror.staticClass(name).toType

  sealed trait Trait1

  sealed trait Trait2 {
    def someFunction : String
  }

  case class TestClass1(name: String) extends Trait2 with Trait1 {
    override def someFunction: String = ???
  }

  case class TestClass2[T](name: T)

  case class TestClass3[T](name: List[T])

  case class TestClass4[T](name: TestClass3[T])

  case class TestClass5[T](name: Option[T])

  case class TestClass6[T](name: Option[TestClass5[List[Option[TestClass4[String]]]]], age: TestClass3[TestClass2[TestClass1]])


}
