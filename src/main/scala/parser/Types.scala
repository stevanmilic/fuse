package parser

import org.parboiled2._

object Types {
  import Identifiers._

  // Type Params
  case class FTypeParam(i: FIdentifier, defaultType: Option[FType] = None)
  type FTypeParamClause = Option[Seq[FTypeParam]]

  sealed trait FType

  // Type Definitions
  case class FSimpleType(
      i: FIdentifier,
      t: Option[Seq[FType]] = None
  ) extends FType
  case class FTupleType(t1: Seq[FType]) extends FType
  case class FFuncType(i: Seq[FType], o: FType) extends FType
  type FTypes = Seq[FType]

  case class FParam(i: FIdentifier, t: FType)
  type FParams = Seq[FParam]

}

abstract class Types extends Identifiers {
  import Types._

  // Type Definitions
  def TypeParam = rule { Id ~ (wspStr("=") ~ Type).? ~> FTypeParam }
  def TypeParamClause = rule { "[" ~ TypeParam.+(",") ~ "]" }

  // NOTE: Recursions exists here through the type arguments. The parser could
  // in theory go into an infite loop, if there is infinite amount of nesting
  // through type args.
  def Type: Rule1[FType] = rule {
    FuncType | TupleType | SimpleType
  }
  def FuncType = {
    def FuncArgs = rule {
      SimpleType ~> (Seq(_)) | "(" ~ SimpleType.*(",") ~ ")"
    }
    rule {
      FuncArgs ~ wspStr("->") ~ Type ~> FFuncType
    }
  }
  def TupleType = rule {
    "(" ~ Type.*(",") ~ ")" ~> FTupleType
  }
  def SimpleType = rule {
    Id ~ TypeArgs.? ~> FSimpleType
  }
  def TypeList = rule { Type.+(",") }
  def TypeArgs = rule { "[" ~ TypeList ~ "]" }

  def Param = rule { Id ~ ":" ~ Type ~> FParam }
  def Params = rule { Param.+(",") }
}
