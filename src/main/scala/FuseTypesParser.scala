package fuse

import org.parboiled2._

object FuseTypesParser {
  import FuseLexicalParser._

  // Type Params
  case class FTypeParam(i: FIdentifier, defaultType: Option[FType] = None)
  type FTypeParamClause = Option[Seq[FTypeParam]]

  sealed trait FType

  // Type Definitions
  case class FSimpleType(
      i: FIdentifier,
      t: Option[Seq[FType]] = None
  ) extends FType
  case class FTupleType(s: Seq[FSimpleType]) extends FType
  case class FFuncType(i: FType, o: FType) extends FType
  type FTypes = Seq[FType]

  case class FParam(i: FIdentifier, t: FType)
  type FParams = Seq[FParam]

  case class FFuncSig(
      isTail: Boolean,
      i: FIdentifier,
      tp: FTypeParamClause,
      p: Option[FParams],
      r: FType
  )

}

abstract class FuseTypesParser extends FuseLexicalParser {
  import FuseTypesParser._
  import FuseLexicalParser._

  // Type Definitions
  def TypeParam = rule { Id ~ (wspStr("=") ~ Type).? ~> FTypeParam }
  def TypeParamClause = rule { "[" ~ TypeParam.+(",") ~ "]" }

  // NOTE: Recursions exists here through the type arguments. The parser could
  // in theory go into an infite loop, if there is infinite amount of nesting
  // through type args.
  def Type: Rule1[FType] = rule {
    FuncType | SimpleType
  }
  // TODO: Handle the case with multiple params.
  def FuncType = rule {
    SimpleType ~ wspStr("->") ~ SimpleType ~> FFuncType
  }
  def SimpleType = rule {
    Id ~ TypeArgs.? ~> FSimpleType
  }
  def Types = rule { Type.+(",") }
  def TypeArgs = rule { "[" ~ Types ~ "]" }

  def Param = rule { Id ~ ":" ~ Type ~> FParam }
  def Params = rule { Param.+(",") }

  def FuncSig = {
    def Tail = rule { capture("tail".?) ~> ((s: String) => !s.isEmpty) }
    rule {
      Tail ~ "def" ~ Id ~ TypeParamClause.? ~ "(" ~ Params.? ~ ")" ~ "->" ~ Type ~> FFuncSig
    }
  }
}
