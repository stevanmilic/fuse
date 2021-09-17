package parser

import org.parboiled2._

object Types {
  import Identifiers._
  import Info._

  // Type Params
  case class FTypeParam(
      info: Info,
      i: FIdentifier,
      defaultType: Option[FType] = None
  )
  type FTypeParamClause = Option[Seq[FTypeParam]]

  sealed trait FType

  implicit val showTypeInfo: ShowInfo[FType] = ShowInfo.info(_ match {
    case FSimpleType(info, _, _) => info
    case FTupleType(info, _)     => info
    case FFuncType(info, _, _)   => info
  })

  // Type Definitions
  case class FSimpleType(
      info: Info,
      i: FIdentifier,
      t: Option[Seq[FType]] = None
  ) extends FType
  case class FTupleType(info: Info, t1: Seq[FType]) extends FType
  case class FFuncType(info: Info, i: Seq[FType], o: FType) extends FType
  type FTypes = Seq[FType]

  case class FParam(info: Info, i: FIdentifier, t: FType)
  type FParams = Seq[FParam]
}

abstract class Types(fileName: String) extends Identifiers(fileName) {
  import Types._

  // Type Definitions
  def TypeParam = rule { info ~ Id ~ (wspStr("=") ~ Type).? ~> FTypeParam }
  def TypeParamClause = rule { "[" ~ TypeParam.+(",") ~ "]" }

  def Type: Rule1[FType] = rule {
    FuncType | TupleType | SimpleType
  }
  def FuncType = {
    def FuncArgs = rule {
      SimpleType ~> (Seq(_)) | "(" ~ SimpleType.*(",") ~ ")"
    }
    rule {
      info ~ FuncArgs ~ wspStr("->") ~ Type ~> FFuncType
    }
  }
  def TupleType = rule {
    info ~ "(" ~ Type.*(",") ~ ")" ~> FTupleType
  }
  def SimpleType = rule {
    info ~ Id ~ TypeArgs.? ~> FSimpleType
  }
  def TypeList = rule { Type.+(",") }
  def TypeArgs = rule { "[" ~ TypeList ~ "]" }

  def Param = rule { info ~ Id ~ ":" ~ Type ~> FParam }
  def Params = rule { Param.+(",") }
}
