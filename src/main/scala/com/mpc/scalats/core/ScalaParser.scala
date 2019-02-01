package com.mpc.scalats.core

/**
  * Created by Milosz on 09.06.2016.
  */

import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._

case class TypeToParse(t: Type, parent: Option[Type]) {

  def typeSymbol = t.typeSymbol

  def members = t.members

  def typeConstructor = t.typeConstructor
}

object ScalaParser {

  private val logger = LoggerFactory.getLogger(getClass)

  private[core] var alreadyExamined: mutable.Set[TypeToParse] = mutable.Set.empty

  import ScalaModel._

  def parseTypes(userInputTypes: List[Type]): List[CaseClass] = {

    val initialTypes = userInputTypes.map(t => TypeToParse(t, None))

    val allInvolvedTypes = initialTypes.flatMap(analyseType).distinct

    allInvolvedTypes.map(parse)

  }

  private def isSealedTrait(scalaType: TypeToParse) = {
    scalaType.typeSymbol.asClass.isSealed &&
      scalaType.typeSymbol.asClass.isTrait
  }

  private def analyseType(analysedType: TypeToParse): List[TypeToParse] = {
    if (!alreadyExamined.exists(x => x.t == analysedType.t)) {
      alreadyExamined.add(analysedType)
      if (isSealedTrait(analysedType)) {
        analysedType :: (directKnownSubclasses(analysedType.t).map(
          t => TypeToParse(t, Some(analysedType.t))
        ).flatMap(analyseType))

      } else if (isCaseClass(analysedType)) {
        analysedType :: getInvolvedTypes(analysedType).flatMap(analyseType)
      } else {
        Nil
      }
    } else {
      Nil
    }
  }

  private def directKnownSubclasses(tpe: Type): List[Type] = {
    // Workaround for SI-7046: https://issues.scala-lang.org/browse/SI-7046
    val tpeSym = tpe.typeSymbol.asClass

    @annotation.tailrec
    def allSubclasses(path: Traversable[Symbol], subclasses: Set[Type]): Set[Type] = path.headOption match {
      case Some(cls: ClassSymbol) if (
        tpeSym != cls && cls.selfType.baseClasses.contains(tpeSym)) => {
        val newSub: Set[Type] = if (!cls.isCaseClass) {
          logger.warn(s"cannot handle class ${cls.fullName}: no case accessor")
          Set.empty
        } else if (cls.typeParams.nonEmpty) {
          logger.warn(s"cannot handle class ${cls.fullName}: type parameter not supported")
          Set.empty
        } else Set(cls.selfType)

        allSubclasses(path.tail, subclasses ++ newSub)
      }

      case Some(o: ModuleSymbol) if (
        o.companion == NoSymbol && // not a companion object
          o.typeSignature.baseClasses.contains(tpeSym)) =>
        allSubclasses(path.tail, subclasses + o.typeSignature)

      case Some(o: ModuleSymbol) if (
        o.companion == NoSymbol // not a companion object
        ) => allSubclasses(path.tail, subclasses)

      case Some(_) => allSubclasses(path.tail, subclasses)

      case _ => subclasses
    }

    if (tpeSym.isSealed && tpeSym.isAbstract) {
      allSubclasses(tpeSym.owner.typeSignature.decls, Set.empty).toList
    } else List.empty
  }

  private def getInvolvedTypes(typeToParse: TypeToParse): List[TypeToParse] = {

    var alreadyExamined : ArrayBuffer[Type] = ArrayBuffer.empty

    def getInvolvedTypes(alreadyExamined: Set[Type])(scalaType: Type): List[Type] = {
      if (!alreadyExamined.contains(scalaType) && !scalaType.typeSymbol.isParameter) {
        val relevantMemberSymbols = scalaType.members.collect {
          case m: MethodSymbol if m.isCaseAccessor => m
        }
        val memberTypes = relevantMemberSymbols.map(_.typeSignature match {
          case NullaryMethodType(resultType) => resultType
          case t => t
        }).flatMap(getInvolvedTypes(alreadyExamined + scalaType))
        val typeArgs = scalaType match {
          case t: scala.reflect.runtime.universe.TypeRef => t.args.flatMap(getInvolvedTypes(alreadyExamined + scalaType))
          case _ => List.empty
        }
        (scalaType.typeConstructor :: typeArgs ::: memberTypes.toList).filter(!_.typeSymbol.isParameter).distinct
      } else {
        List.empty
      }
    }

    getInvolvedTypes(Set.empty)(typeToParse.t).map(t => TypeToParse(t, None))

    }


  private def parse(typeToParse: TypeToParse): CaseClass = {
    if (isSealedTrait(typeToParse)) {
      parseSealedTrait(typeToParse)
    } else if (isCaseClass(typeToParse)) {
      parseCaseClass(typeToParse)
    } else {
      throw new Exception("Failure")
    }
  }

  private def parseSealedTrait(sealedTraitType: TypeToParse): CaseClass = {
    val typeParams = sealedTraitType.typeConstructor.normalize match {
      case polyType: PolyTypeApi => polyType.typeParams.map(_.name.decoded)
      case _ => List.empty[String]
    }
    CaseClass(
      sealedTraitType.typeSymbol.name.toString,
      Nil,
      typeParams
    )
  }

  private def parseCaseClass(caseClassType: TypeToParse): CaseClass = {

    alreadyExamined += caseClassType

    val relevantMemberSymbols = caseClassType.members.collect {
      case m: MethodSymbol if m.isCaseAccessor => m
    }
    val typeParams = caseClassType.typeConstructor.normalize match {
      case polyType: PolyTypeApi => polyType.typeParams.map(_.name.decoded)
      case _ => List.empty[String]
    }
    val members = relevantMemberSymbols map { member =>
      val memberName = member.name.toString
      CaseClassMember(memberName, getTypeRef(member.returnType, typeParams.toSet))
    }

    CaseClass(
      caseClassType.typeSymbol.name.toString,
      members.toList,
      typeParams,
      caseClassType.parent.map(_.typeSymbol.name.toString)
    )
  }

  private def getTypeRef(scalaType: Type, typeParams: Set[String]): TypeRef = {
    val typeName = scalaType.typeSymbol.name.toString
    typeName match {
      case "Int" | "Byte" =>
        IntRef
      case "Long" =>
        LongRef
      case "Double" =>
        DoubleRef
      case "Float" =>
        FloatRef
      case "Boolean" =>
        BooleanRef
      case "String" | "UUID" =>
        StringRef
      case "List" | "Seq" | "Set" =>
        val innerType = scalaType.asInstanceOf[scala.reflect.runtime.universe.TypeRef].args.head
        SeqRef(getTypeRef(innerType, typeParams))
      case "Option" =>
        val innerType = scalaType.asInstanceOf[scala.reflect.runtime.universe.TypeRef].args.head
        OptionRef(getTypeRef(innerType, typeParams))
      case "LocalDate" =>
        DateRef
      case "Instant" | "Timestamp" =>
        DateTimeRef
      case typeParam if typeParams.contains(typeParam) =>
        TypeParamRef(typeParam)
      case _ if isCaseClass(scalaType) =>
        val caseClassName = scalaType.typeSymbol.name.toString
        val typeArgs = scalaType.asInstanceOf[scala.reflect.runtime.universe.TypeRef].args
        val typeArgRefs = typeArgs.map(getTypeRef(_, typeParams))
        CaseClassRef(caseClassName, typeArgRefs)
      case _ =>
        UnknownTypeRef(typeName)
    }
  }

  private def isCaseClass(scalaType: Type): Boolean =
    scalaType.members.collect({ case m: MethodSymbol if m.isCaseAccessor => m }).nonEmpty

  private def isCaseClass(scalaType: TypeToParse): Boolean = isCaseClass(scalaType.t)
}