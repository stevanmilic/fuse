package parser

import org.parboiled2._
import scala.util.Either

object FuseParser {
  import Identifiers._
  import Types._
  import Expressions.FExpr
  import Info._

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
      type_id: FIdentifier,
      type_tp: FTypeParamClause,
      trait_id: FIdentifier,
      trait_tp: FTypeParamClause,
      f: Seq[FFuncDecl]
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
  import FuseParser._
  import Identifiers._
  import Types._
  import Expressions.FExpr

  def Module = rule { Decl.+(NewLine.+) ~ quiet(WL) ~ quiet(EOI) }
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
  def PrimitiveTypeDecl = rule { TypeDecl ~> FPrimitiveTypeDecl }

  def VariantTypeDecl = {
    def VariantTypeValueArgs = rule {
      "(" ~ (params ~> (Left(_)) | TypeList.named("types") ~> (Right(_))) ~ ")"
    }

    val VariantTypeValue = () =>
      rule { info ~ identifier ~ VariantTypeValueArgs.? ~> FVariantTypeValue }

    rule {
      TypeDecl ~ TypeParamClause.? ~ `:` ~ oneOrMoreWithIndent(
        VariantTypeValue,
        "value"
      ) ~> FVariantTypeDecl
    }
  }

  def RecordTypeDecl = {
    val RecordTypeField = () => rule { param ~> FRecordTypeField }
    rule {
      TypeDecl ~ TypeParamClause.named("type params").? ~ `:` ~
        oneOrMoreWithIndent(RecordTypeField, "field") ~> FRecordTypeDecl
    }
  }

  def TupleTypeDecl = rule {
    TypeDecl ~ TypeParamClause.? ~ "(" ~ TypeList.named(
      "types"
    ) ~ ")" ~> FTupleTypeDecl
  }

  def TypeAlias = rule {
    TypeDecl ~ TypeParamClause.? ~ `=` ~ Type ~> FTypeAlias
  }

  def FuncSig = {
    rule {
      `fun` ~ info ~ identifier ~ TypeParamClause
        .named("type params")
        .? ~ "(" ~ params
        .named("parameters")
        .? ~ ")" ~ `->` ~ Type ~> FFuncSig
    }
  }

  def FuncDecl = {
    rule {
      FuncSig ~ BlockExpr ~> FFuncDecl
    }
  }

  def TraitDecl = {
    val TraitFunc = () =>
      rule { FuncSig ~ ";" ~> (Right(_)) | FuncDecl ~> (Left(_)) }
    rule {
      `trait` ~ info ~ identifier ~ TypeParamClause.? ~ ":" ~ oneOrMoreWithIndent(
        TraitFunc,
        "function"
      ) ~>
        FTraitDecl
    }
  }

  val TypeFunc = () => FuncDecl
  def ImplFuncDecls = oneOrMoreWithIndent(TypeFunc, "function")
  def TraitInstance = rule {
    `impl` ~ info ~ identifier ~ TypeParamClause.? ~ `for` ~ identifier ~ TypeParamClause.? ~ `:` ~ ImplFuncDecls ~> FTraitInstance
  }

  def TypeFuncDecls = rule {
    `impl` ~ info ~ identifier ~ TypeParamClause.? ~ `:` ~ ImplFuncDecls ~> FTypeFuncDecls
  }
}
