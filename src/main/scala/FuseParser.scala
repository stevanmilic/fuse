package fuse

import org.parboiled2._
import scala.util.Either

object FuseParser {
  import FuseLexicalParser._
  import FuseTypesParser._
  import FuseExpressionParser.FExpr

  sealed trait FNode

  case class FPrimitiveType(t: FIdentifier) extends FNode

  case class FSumTypeValue(
      v: FIdentifier,
      t: Option[Either[FParams, FTypes]] = None
  )
  case class FSumType(
      i: FIdentifier,
      t: FTypeParamClause,
      values: Seq[FSumTypeValue]
  ) extends FNode

  case class FStructTypeField(p: FParam)
  case class FStructType(
      i: FIdentifier,
      t: FTypeParamClause,
      fields: Seq[FStructTypeField]
  ) extends FNode

  case class FTupleTypeDef(
      i: FIdentifier,
      t: FTypeParamClause,
      types: FTypes
  ) extends FNode

  case class FTypeAlias(
      i: FIdentifier,
      t: FType
  ) extends FNode

  // Function defininition
  case class FFuncDef(
      sig: FFuncSig,
      exprs: Seq[FExpr]
  ) extends FNode

  // Trait definition + Implementations
  case class FTrait(
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
    StructType |
      SumType |
      TupleType |
      TypeAlias |
      PrimitiveType |
      FuncDef |
      Trait |
      TraitImpl |
      TypeImpl
  }

  def TypeDef = rule { "type" ~ Id }
  def PrimitiveType = rule { TypeDef ~> FPrimitiveType }

  def SumType = {
    def SumTypeValueArgs = rule {
      "(" ~ (Params ~> (Left(_)) | Types ~> (Right(_))) ~ ")"
    }
    val SumTypeValue = () => rule { Id ~ SumTypeValueArgs.? ~> FSumTypeValue }
    rule {
      TypeDef ~ TypeParamClause.? ~ ":" ~ oneOrMoreWithIndent(SumTypeValue) ~>
        FSumType
    }
  }

  def StructType = {
    val StructTypeField = () => rule { Param ~> FStructTypeField }
    rule {
      TypeDef ~ TypeParamClause.? ~ ":" ~
        oneOrMoreWithIndent(StructTypeField) ~> FStructType
    }
  }

  def TupleType = rule {
    TypeDef ~ TypeParamClause.? ~ "(" ~ Types ~ ")" ~> FTupleTypeDef
  }

  def TypeAlias = rule { TypeDef ~ wspStr("=") ~ Type ~> FTypeAlias }

  // Func definition
  def FuncDef = {
    def BlockExpr = rule { runSubParser(new FuseExpressionParser(_).BlockExpr) }
    rule {
      FuncSig ~ ":" ~ BlockExpr ~> FFuncDef
    }
  }

  // Trait definition + Implementations
  def Trait = {
    val TraitFunc = () => rule { FuncDef ~> (Left(_)) | FuncSig ~> (Right(_)) }
    rule {
      "trait" ~ Id ~ TypeParamClause.? ~ ":" ~ oneOrMoreWithIndent(TraitFunc) ~>
        FTrait
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
