package parser

import org.parboiled2._
import scala.util.Either

object FuseParser {
  import Identifiers._
  import Types._
  import Expressions.FExpr

  sealed trait FDecl

  case class FPrimitiveTypeDecl(t: FIdentifier) extends FDecl

  case class FVariantTypeValue(
      v: FIdentifier,
      t: Option[Either[FParams, FTypes]] = None
  )
  case class FVariantTypeDecl(
      i: FIdentifier,
      t: FTypeParamClause,
      values: Seq[FVariantTypeValue]
  ) extends FDecl

  case class FRecordTypeField(p: FParam)
  case class FRecordTypeDecl(
      i: FIdentifier,
      t: FTypeParamClause,
      fields: Seq[FRecordTypeField]
  ) extends FDecl

  case class FTupleTypeDecl(
      i: FIdentifier,
      t: FTypeParamClause,
      types: FTypes
  ) extends FDecl

  case class FTypeAlias(
      i: FIdentifier,
      tp: FTypeParamClause,
      t: FType
  ) extends FDecl

  case class FFuncSig(
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
      i: FIdentifier,
      tp: FTypeParamClause,
      f: Seq[FFuncDecl]
  ) extends FDecl

  case class FTraitDecl(
      i: FIdentifier,
      tp: FTypeParamClause,
      f: Seq[Either[FFuncDecl, FFuncSig]]
  ) extends FDecl

  case class FTraitInstance(
      type_id: FIdentifier,
      type_tp: FTypeParamClause,
      trait_id: FIdentifier,
      trait_tp: FTypeParamClause,
      f: Seq[FFuncDecl]
  ) extends FDecl

}

class FuseParser(val input: ParserInput) extends Types {
  import FuseParser._
  import Identifiers._
  import Types._
  import Expressions.FExpr

  def Module = rule { Decl.+('\n') ~ EOI }
  def Decl: Rule1[FDecl] = rule {
    RecordTypeDecl |
      VariantTypeDecl |
      TupleTypeDecl |
      TypeAlias |
      PrimitiveTypeDecl |
      FuncDecl |
      TraitDecl |
      TraitInstance |
      TypeFuncDecls
  }

  def TypeDecl = rule { "type" ~ Id }
  def PrimitiveTypeDecl = rule { TypeDecl ~> FPrimitiveTypeDecl }

  def VariantTypeDecl = {
    def VariantTypeValueArgs = rule {
      "(" ~ (Params ~> (Left(_)) | TypeList ~> (Right(_))) ~ ")"
    }
    val VariantTypeValue = () =>
      rule { Id ~ VariantTypeValueArgs.? ~> FVariantTypeValue }
    rule {
      TypeDecl ~ TypeParamClause.? ~ ":" ~ oneOrMoreWithIndent(
        VariantTypeValue
      ) ~>
        FVariantTypeDecl
    }
  }

  def RecordTypeDecl = {
    val RecordTypeField = () => rule { Param ~> FRecordTypeField }
    rule {
      TypeDecl ~ TypeParamClause.? ~ ":" ~
        oneOrMoreWithIndent(RecordTypeField) ~> FRecordTypeDecl
    }
  }

  def TupleTypeDecl = rule {
    TypeDecl ~ TypeParamClause.? ~ "(" ~ TypeList ~ ")" ~> FTupleTypeDecl
  }

  def TypeAlias = rule {
    TypeDecl ~ TypeParamClause.? ~ wspStr("=") ~ Type ~> FTypeAlias
  }

  def FuncSig = {
    rule {
      "fun" ~ Id ~ TypeParamClause.? ~ "(" ~ Params.? ~ ")" ~ "->" ~ Type ~> FFuncSig
    }
  }

  def FuncDecl = {
    def BlockExpr = rule { runSubParser(new Expressions(_).BlockExpr) }
    rule {
      FuncSig ~ BlockExpr ~> FFuncDecl
    }
  }

  def TraitDecl = {
    val TraitFunc = () =>
      rule { FuncSig ~ ";" ~> (Right(_)) | FuncDecl ~> (Left(_)) }
    rule {
      "trait" ~ Id ~ TypeParamClause.? ~ ":" ~ oneOrMoreWithIndent(TraitFunc) ~>
        FTraitDecl
    }
  }

  val TypeFunc = () => FuncDecl
  def ImplFuncDecls = oneOrMoreWithIndent(TypeFunc)
  def TraitInstance = rule {
    "impl" ~ Id ~ TypeParamClause.? ~ wspStr("for") ~ Id ~ TypeParamClause.? ~
      ':' ~ ImplFuncDecls ~> FTraitInstance
  }

  def TypeFuncDecls = rule {
    "impl" ~ Id ~ TypeParamClause.? ~ ":" ~ ImplFuncDecls ~> FTypeFuncDecls
  }
}
