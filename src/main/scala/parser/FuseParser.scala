package parser

import org.parboiled2.*
import scala.util.Either

object FuseParser {
  import Identifiers.*
  import Types.*
  import Expressions.FExpr
  import Info.*

  sealed trait FDecl

  case class FPrimitiveTypeDecl(info: Info, t: FIdentifier) extends FDecl

  case class FVariantTypeValue(
      info: Info,
      v: FIdentifier,
      t: Option[Either[FParams, FTypes]] = None
  )
  case class FVariantTypeDecl(
      info: Info,
      i: FIdentifier,
      t: FTypeParamClause,
      values: Seq[FVariantTypeValue]
  ) extends FDecl

  case class FRecordTypeField(p: FParam)
  case class FRecordTypeDecl(
      info: Info,
      i: FIdentifier,
      t: FTypeParamClause,
      fields: Seq[FRecordTypeField]
  ) extends FDecl

  case class FTupleTypeDecl(
      info: Info,
      i: FIdentifier,
      t: FTypeParamClause,
      types: FTypes
  ) extends FDecl

  case class FTypeAlias(
      info: Info,
      i: FIdentifier,
      tp: FTypeParamClause,
      t: FType
  ) extends FDecl

  case class FFuncSig(
      info: Info,
      i: FIdentifier,
      tp: FTypeParamClause,
      p: Option[FParams],
      r: FType
  )

  case class FFuncDecl(
      sig: FFuncSig,
      exprs: Seq[FExpr]
  ) extends FDecl

  case class FTypeFuncDecls(
      info: Info,
      i: FIdentifier,
      tp: FTypeParamClause,
      f: Seq[FFuncDecl]
  ) extends FDecl

  case class FTraitDecl(
      info: Info,
      i: FIdentifier,
      tp: FTypeParamClause,
      f: Seq[Either[FFuncDecl, FFuncSig]]
  ) extends FDecl

  case class FTraitInstance(
      info: Info,
      traitIdentifier: FIdentifier,
      traitParams: FTypeParamClause,
      typeIdentifier: FIdentifier,
      typeParams: FTypeParamClause,
      methods: Seq[FFuncDecl]
  ) extends FDecl

  implicit val showDeclInfo: ShowInfo[FDecl] = ShowInfo.info(_ match {
    case FPrimitiveTypeDecl(info, _)              => info
    case FVariantTypeDecl(info, _, _, _)          => info
    case FRecordTypeDecl(info, _, _, _)           => info
    case FTupleTypeDecl(info, _, _, _)            => info
    case FTypeAlias(info, _, _, _)                => info
    case FFuncDecl(FFuncSig(info, _, _, _, _), _) => info
    case FTypeFuncDecls(info, _, _, _)            => info
    case FTraitDecl(info, _, _, _)                => info
    case FTraitInstance(info, _, _, _, _, _)      => info

  })
}

class FuseParser(val input: ParserInput, fileName: String)
    extends Expressions(fileName) {
  import FuseParser.*
  import Identifiers.*
  import Types.*
  import Expressions.FExpr

  def Module = rule { Decl.+(EmptyLine.+) ~ quiet(WL) ~ quiet(EOI) }
  def Decl: Rule1[FDecl] = rule {
    RecordTypeDecl.named("record") |
      VariantTypeDecl.named("variant") |
      TupleTypeDecl.named("tuple") |
      TypeAlias.named("type alias") |
      PrimitiveTypeDecl.named("primitive type") |
      FuncDecl.named("function") |
      TraitDecl.named("trait") |
      TraitInstance.named("instance") |
      TypeFuncDecls.named("type methods")
  }

  def TypeDecl = rule { `type` ~ info ~ identifier }
  def PrimitiveTypeDecl = rule { TypeDecl ~> FPrimitiveTypeDecl.apply }

  def VariantTypeDecl = {
    def VariantTypeValueArgs = rule {
      "(" ~ (params ~> (Left(_)) | TypeList.named("types") ~> (Right(_))) ~ ")"
    }

    val VariantTypeValue = () =>
      rule {
        info ~ identifier ~ VariantTypeValueArgs.? ~> FVariantTypeValue.apply
      }

    rule {
      TypeDecl ~ TypeParamClause.? ~ `:` ~ oneOrMoreWithIndent(
        VariantTypeValue,
        "value"
      ) ~> FVariantTypeDecl.apply
    }
  }

  def RecordTypeDecl = {
    val RecordTypeField = () => rule { param ~> FRecordTypeField.apply }
    rule {
      TypeDecl ~ TypeParamClause.named("type params").? ~ `:` ~
        oneOrMoreWithIndent(RecordTypeField, "field") ~> FRecordTypeDecl.apply
    }
  }

  def TupleTypeDecl = rule {
    TypeDecl ~ TypeParamClause.? ~ "(" ~ TypeList.named(
      "types"
    ) ~ ")" ~> FTupleTypeDecl.apply
  }

  def TypeAlias = rule {
    TypeDecl ~ TypeParamClause.? ~ `=` ~ Type ~> FTypeAlias.apply
  }

  def FuncSig = {
    rule {
      `fun` ~ info ~ identifier ~ TypeParamClause
        .named("type params")
        .? ~ "(" ~ params
        .named("parameters")
        .? ~ ")" ~ `->` ~ Type ~> FFuncSig.apply
    }
  }

  def FuncDecl = {
    rule {
      FuncSig ~ BlockExpr ~> FFuncDecl.apply
    }
  }

  def TraitDecl = {
    val TraitFunc = () =>
      rule { FuncSig ~ ";" ~> (Right(_)) | FuncDecl ~> (Left(_)) }
    rule {
      `trait` ~ info ~ identifier ~ TypeParamClause.? ~ `:` ~ oneOrMoreWithIndent(
        TraitFunc,
        "function"
      ) ~>
        FTraitDecl.apply
    }
  }

  val TypeFunc = () => FuncDecl
  def ImplFuncDecls = oneOrMoreWithIndent(TypeFunc, "function")
  def TraitInstance = rule {
    `impl` ~ info ~ identifier ~ TypeParamClause.? ~ `for` ~ identifier ~ TypeParamClause.? ~ `:` ~ ImplFuncDecls ~> FTraitInstance.apply
  }

  def TypeFuncDecls = rule {
    `impl` ~ info ~ identifier ~ TypeParamClause.? ~ `:` ~ ImplFuncDecls ~> FTypeFuncDecls.apply
  }
}
