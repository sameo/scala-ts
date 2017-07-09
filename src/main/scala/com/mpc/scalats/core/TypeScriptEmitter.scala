package com.mpc.scalats.core

import java.io.PrintStream

import com.mpc.scalats.core.TypeScriptModel.AccessModifier.{Private, Public}
import com.mpc.scalats.core.TypeScriptModel.{DateRef, DateTimeRef}

object TypeScriptEmitter {

  import TypeScriptModel._

  def emit(declaration: List[Declaration], out: PrintStream): Unit = {
    declaration foreach {
      case decl: InterfaceDeclaration =>
        emitInterfaceDeclaration(decl, out)
      case decl: ClassDeclaration =>
        emitClassDeclaration(decl, out)
    }
  }

  private def emitInterfaceDeclaration(decl: InterfaceDeclaration, out: PrintStream) = {
    val InterfaceDeclaration(name, members, typeParams) = decl
    out.print(s"export interface $name")
    emitTypeParams(decl.typeParams, out)
    out.println(" {")
    members foreach { member =>
      out.println(s"\t${member.name}: ${getTypeRefString(member.typeRef)}")
    }
    out.println("}")
    out.println()
  }

  private def emitClassDeclaration(decl: ClassDeclaration, out: PrintStream) = {
    val ClassDeclaration(name, ClassConstructor(parameters), typeParams) = decl
    out.print(s"export class $name implements I$name")
    emitTypeParams(decl.typeParams, out)
    out.println(" {")

    emitClassProperties(parameters, out)
    emitClassConstructor(name, out)
    emitPropertiesParser(name, parameters, out)
    emitPropertiesExporter(name, parameters, out)

    out.println("}")
    out.println()
  }

  private def emitClassProperties(parameters: List[ClassConstructorParameter], out: PrintStream) {
    parameters.foreach(p => {
      val accessModifier = p.accessModifier match {
        case Some(Public) => "public "
        case Some(Private) => "private _"
        case None => ""
      }
      out.println(s"\t$accessModifier${p.name}: ${getTypeRefString(p.typeRef)}")
    })
    out.println()
  }

  private def emitClassConstructor(name: String, out: PrintStream) {
    out.println(s"\tconstructor(data?: I$name) {")
    out.println("\t\tthis._parse(data)")
    out.println("\t}")
    out.println()
  }

  private def emitPropertiesParser(name: String, parameters: List[ClassConstructorParameter], out: PrintStream) {
    out.println(s"\tpublic parse(data?: I$name) {")
    parameters.foreach(p => {
      out.println(s"\t\tthis.${p.name} = ${getTypeParseString(p.typeRef, Some("data"), Some(p.name), None)}")
    })
    out.println("\t}")
    out.println()
  }

  private def emitPropertiesExporter(name: String, parameters: List[ClassConstructorParameter], out: PrintStream) {
    out.println(s"\tpublic toJSON(): I$name {")
    out.println("\t\treturn {")
    parameters.zipWithIndex.foreach({
      case (p, index) => {
        val endLine = if (index + 1 < parameters.length) "," else ""
        out.println(s"\t\t\t${p.name}: ${getTypeExportString(p.typeRef, Some("this"), Some(p.name))}$endLine")
      }
    })
    out.println("\t\t}")
    out.println("\t}")
    out.println()
  }

  private def emitTypeParams(params: List[String], out: PrintStream) =
    if (params.nonEmpty) {
      out.print("<")
      out.print(params.mkString(", "))
      out.print(">")
    }

  private def getTypeRefString(typeRef: TypeRef): String = typeRef match {
    case NumberRef => "number"
    case BooleanRef => "boolean"
    case StringRef => "string"
    case DateRef | DateTimeRef => "Date"
    case ArrayRef(innerType) => s"Array<${getTypeRefString(innerType)}>"
    case CustomTypeRef(name, params) if params.isEmpty => name
    case CustomTypeRef(name, params) if params.nonEmpty =>
      s"$name<${params.map(getTypeRefString).mkString(", ")}>"
    case UnknownTypeRef(typeName) => typeName
    case TypeParamRef(param) => param
    case UnionType(inner1, inner2) => s"${getTypeRefString(inner1)} | ${getTypeRefString(inner2)}"
    case NullRef => "null"
    case UndefinedRef => "undefined"
  }

  private def getTypeExportString(typeRef: TypeRef, context: Option[String], property: Option[String]): String = {
    val value = getValue(context, property, None)
    typeRef match {
      case NullRef => "null"
      case UndefinedRef => "undefined"
      case DateRef | DateTimeRef => s"moment($value).unix() * 1000"
      case ArrayRef(innerType) => s"_.map($value || [], v => ${getTypeExportString(innerType, Some("v"), None)})"
      case UnionType(inner1, inner2) => (inner1, inner2) match {
        case (NullRef, _) => s"_.isNil($value) ? null : ${getTypeExportString(inner2, context, property)}"
        case (_, NullRef) => s"_.isNil($value) ? null : ${getTypeExportString(inner1, context, property)}"
      }
      case CustomTypeRef(name, params) => s"$value.toJSON()"
      case _ => value
    }
  }

  private def getTypeParseString(typeRef: TypeRef, context: Option[String], property: Option[String], defaultValue: Option[String]): String = {
    val value = getValue(context, property, defaultValue)
    typeRef match {
      case NullRef => "null"
      case UndefinedRef => "undefined"
      case DateRef | DateTimeRef => s"moment($value).toDate()"
      case ArrayRef(innerType) => s"_.map($value || [], v => ${getTypeParseString(innerType, Some("v"), None, None)})"
      case CustomTypeRef(name, params) => s"new $name($value)"
      case UnionType(inner1, inner2) => (inner1, inner2) match {
        case (NullRef, _) => getTypeParseString(inner2, context, property, Some("null"))
        case (_, NullRef) => getTypeParseString(inner1, context, property, Some("null"))
      }
      case _ => value
    }
  }

  private def getValue(context: Option[String], property: Option[String], defaultValue: Option[String]): String = (context, property) match {
    case (Some(c), Some(p)) => {
      val default = defaultValue match {
        case Some(d) => s", $d"
        case None => ""
      }
      if (c.equals("this")) {
        s"$c.$p"
      } else {
        s"_.get($c, '$p'$default)"
      }
    }
    case (Some(c), None) => c
    case _ => property.get
  }
}
