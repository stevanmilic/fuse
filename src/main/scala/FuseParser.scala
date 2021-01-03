package fuse

import org.parboiled2._
import scala.util.Either

object FuseParser {
  // Abstract syntax tree model.
  sealed trait FNode

  // Type Params
  case class FTypeParam(i: FIdentifier, defaultType: Option[FType] = None)
      extends FNode
  type FTypeParamClause = Option[Seq[FTypeParam]]

  // Type Definitions
  case class FType(i: FIdentifier, t: Option[Seq[FType]] = None)
      extends FNode // TODO: Extend to support function type.
  type FTypes = Seq[FType]

  case class FParam(i: FIdentifier, t: FType)
  type FParams = Seq[FParam]

  case class FPrimitiveType(t: FIdentifier) extends FNode

  case class FSumTypeValue(
      v: FIdentifier,
      t: Option[Either[FParams, FTypes]] = None
  ) extends FNode
  case class FSumType(
      i: FIdentifier,
      t: FTypeParamClause,
      values: Seq[FSumTypeValue]
  ) extends FNode

  case class FStructTypeField(p: FParam) extends FNode
  case class FStructType(
      i: FIdentifier,
      t: FTypeParamClause,
      fields: Seq[FStructTypeField]
  ) extends FNode

  case class FTupleType(
      i: FIdentifier,
      t: FTypeParamClause,
      types: FTypes
  ) extends FNode

  // Function defininition
  case class FFuncSig(
      isTail: Boolean,
      i: FIdentifier,
      tp: FTypeParamClause,
      p: Option[FParams],
      r: FType
  ) extends FNode

  // Trait definition + Implementations

  case class FTrait(
      i: FIdentifier,
      tp: FTypeParamClause,
      f: Seq[FFuncSig]
  ) extends FNode

  // Lexical tokens
  case class FIdentifier(value: String) extends FNode
  case class FIndent(size: Int) extends FNode
  case object FBadIndent extends FNode

  def extractNodesWithIndent[T <: FNode](
      lines: Seq[(FIndent, T)]
  ): Option[Seq[T]] = {
    val (indents, nodes) = lines.unzip
    indents.forall(_.size == indents.head.size) match {
      case true  => Some(nodes)
      case false => None
    }
  }
}

class FuseParser(val input: ParserInput) extends Parser {
  import FuseParser._
  import CharPredicate.{AlphaNum}
  implicit def wspStrL(s: String): Rule0 = rule {
    str(s) ~ zeroOrMore(' ')
  }

  def wspStr(s: String): Rule0 = rule {
    zeroOrMore(' ') ~ wspStrL(s)
  }

  def InputLine = rule(Program ~ EOI)
  def Program: Rule1[FNode] = rule(
    StructTypeDef | SumTypeDef | TupleTypeDef | PrimitiveTypeDef | FuncSig | Trait
  )

  // Type Definitions
  def TypeParam = rule(Id ~ (wspStr("=") ~ Type).? ~> FTypeParam)
  def TypeParamClause = rule(
    "[" ~ TypeParam ~ ("," ~ TypeParam).* ~ "]" ~> (_ +: _)
  )

  // NOTE: Recursions exists here through the type arguments. The parser could
  // in theory go into an infite loop, if there is infinite amount of nesting
  // through type args.
  def Type: Rule1[FType] = rule(Id ~ TypeArgs.? ~> FType)
  def Types = rule(Type ~ ("," ~ Type).* ~> (_ +: _))
  def TypeArgs = rule("[" ~ Types ~ "]")

  def Param = rule((Id ~ ":" ~ Type) ~> FParam)
  def Params = rule(Param ~ ("," ~ Param).* ~> (_ +: _))

  def TypeDef = rule("type" ~ Id)
  def PrimitiveTypeDef = rule(TypeDef ~> FPrimitiveType)

  def SumTypeValueTypes = rule(Types ~> (Right(_)))
  def SumTypeValueParams = rule(Params ~> (Left(_)))
  def SumTypeValueArgs: Rule1[Either[FParams, FTypes]] = rule(
    "(" ~ (SumTypeValueParams | SumTypeValueTypes) ~ ")"
  )
  def SumTypeValue = rule(
    (NewLine ~ Indent ~ Id ~ SumTypeValueArgs.?) ~> ((i, v, t) =>
      (i, FSumTypeValue(v, t))
    )
  )
  def SumTypeDef = rule(
    (TypeDef ~ TypeParamClause.? ~ ":" ~ SumTypeValue.+) ~> ((i, t, lines) =>
      extractNodesWithIndent(lines)
        .map(FSumType(i, t, _))
        .getOrElse(FBadIndent)
    )
  )

  def StructTypeField = rule(
    NewLine ~ Indent ~ Param ~> ((i, p) => (i, FStructTypeField(p)))
  )
  def StructTypeDef = rule(
    (TypeDef ~ TypeParamClause.? ~ ":" ~ StructTypeField.+) ~> ((i, t, lines) =>
      extractNodesWithIndent(lines)
        .map(FStructType(i, t, _))
        .getOrElse(FBadIndent)
    )
  )

  def TupleTypeDef = rule(
    (TypeDef ~ TypeParamClause.? ~ "(" ~ Types ~ ")") ~> FTupleType
  )

  // Func definition
  def Tail = rule(capture("tail".?) ~> ((s: String) => !s.isEmpty))
  def FuncSig = rule(
    Tail ~ "def" ~ Id ~ TypeParamClause.? ~ "(" ~ Params.? ~ ")" ~ "->" ~ Type ~> FFuncSig
  )

  // Trait definition + Implementations

  // TODO: Handle Func Definitions.
  def TraitFuncSig = rule(NewLine ~ Indent ~ FuncSig ~> ((_, _)))
  def Trait = rule(
    "trait" ~ Id ~ TypeParamClause.? ~ ":" ~ TraitFuncSig.+ ~> ((i, t, lines) =>
      extractNodesWithIndent(lines).map(FTrait(i, t, _)).getOrElse(FBadIndent)
    )
  )

  // Lexical tokens
  def Id = rule(capture(IdentifierPart) ~> FIdentifier)
  def IdentifierPart = rule((AlphaNum | '_').+)
  def NewLine = rule(ch('\n'))
  def Indent = rule(capture(Spacing.+) ~> (s => FIndent(s.size)))
  def Spacing = rule(ch('\t') | ch(' '))
}
