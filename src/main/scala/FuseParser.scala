package fuse

import org.parboiled2._
import scala.util.Either

object FuseParser {
  import FuseLexicalParser._
  import FuseTypesParser._
  import FuseExpressionParser.FExpr

  sealed trait FNode

  case class FPrimitiveTypeDef(t: FIdentifier) extends FNode

  case class FVariantTypeValue(
      v: FIdentifier,
      t: Option[Either[FParams, FTypes]] = None
  )
  case class FVariantTypeDef(
      i: FIdentifier,
      t: FTypeParamClause,
      values: Seq[FVariantTypeValue]
  ) extends FNode

  case class FRecordTypeField(p: FParam)
  case class FRecordTypeDef(
      i: FIdentifier,
      t: FTypeParamClause,
      fields: Seq[FRecordTypeField]
  ) extends FNode

  case class FTupleTypeDef(
      i: FIdentifier,
      t: FTypeParamClause,
      types: FTypes
  ) extends FNode

  case class FTypeAlias(
      i: FIdentifier,
      tp: FTypeParamClause,
      t: FType
  ) extends FNode

  // Function defininition
  case class FFuncDef(
      sig: FFuncSig,
      exprs: Seq[FExpr]
  ) extends FNode

  // Trait definition + Implementations
  case class FTraitDef(
      i: FIdentifier,
      tp: FTypeParamClause,
      f: Seq[Either[FFuncDef, FFuncSig]]
  ) extends FNode

  case class FTraitImpl(
      type_id: FIdentifier,
      type_tp: FTypeParamClause,
      trait_id: FIdentifier,
      trait_tp: FTypeParamClause,
      f: Seq[FFuncDef]
  ) extends FNode

  case class FTypeImpl(
      i: FIdentifier,
      tp: FTypeParamClause,
      f: Seq[FFuncDef]
  ) extends FNode
}

class FuseParser(val input: ParserInput) extends FuseTypesParser {
  import FuseParser._
  import FuseLexicalParser._
  import FuseTypesParser._
  import FuseExpressionParser.FExpr

  def InputLine = rule { Program ~ EOI }
  def Program: Rule1[FNode] = rule {
    RecordTypeDef |
      VariantTypeDef |
      TupleTypeDef |
      TypeAlias |
      PrimitiveTypeDef |
      FuncDef |
      TraitDef |
      TraitImpl |
      TypeImpl
  }

  def TypeDef = rule { "type" ~ Id }
  def PrimitiveTypeDef = rule { TypeDef ~> FPrimitiveTypeDef }

  def VariantTypeDef = {
    def VariantTypeValueArgs = rule {
      "(" ~ (Params ~> (Left(_)) | Types ~> (Right(_))) ~ ")"
    }
    val VariantTypeValue = () =>
      rule { Id ~ VariantTypeValueArgs.? ~> FVariantTypeValue }
    rule {
      TypeDef ~ TypeParamClause.? ~ ":" ~ oneOrMoreWithIndent(
        VariantTypeValue
      ) ~>
        FVariantTypeDef
    }
  }

  def RecordTypeDef = {
    val RecordTypeField = () => rule { Param ~> FRecordTypeField }
    rule {
      TypeDef ~ TypeParamClause.? ~ ":" ~
        oneOrMoreWithIndent(RecordTypeField) ~> FRecordTypeDef
    }
  }

  def TupleTypeDef = rule {
    TypeDef ~ TypeParamClause.? ~ "(" ~ Types ~ ")" ~> FTupleTypeDef
  }

  def TypeAlias = rule {
    TypeDef ~ TypeParamClause.? ~ wspStr("=") ~ Type ~> FTypeAlias
  }

  def FuncDef = {
    def BlockExpr = rule { runSubParser(new FuseExpressionParser(_).BlockExpr) }
    rule {
      FuncSig ~ ":" ~ BlockExpr ~> FFuncDef
    }
  }

  def TraitDef = {
    val TraitFunc = () => rule { FuncDef ~> (Left(_)) | FuncSig ~> (Right(_)) }
    rule {
      "trait" ~ Id ~ TypeParamClause.? ~ ":" ~ oneOrMoreWithIndent(TraitFunc) ~>
        FTraitDef
    }
  }

  def ImplFuncDefs = oneOrMoreWithIndent(() => FuncDef)
  def TraitImpl = rule {
    "impl" ~ Id ~ TypeParamClause.? ~ wspStr("for") ~ Id ~ TypeParamClause.? ~
      ':' ~ ImplFuncDefs ~> FTraitImpl
  }

  def TypeImpl = rule {
    "impl" ~ Id ~ TypeParamClause.? ~ ":" ~ ImplFuncDefs ~> FTypeImpl
  }
}
