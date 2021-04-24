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

  case class FFuncDef(
      sig: FFuncSig,
      exprs: Seq[FExpr]
  ) extends FNode

  // Trait definition + Implementations
  case class FTrait(
      i: FIdentifier,
      tp: FTypeParamClause,
      f: Seq[FFuncSig]
  ) extends FNode

  case class FTraitImpl(
      type_id: FIdentifier,
      type_tp: FTypeParamClause,
      trait_id: FIdentifier,
      trait_tp: FTypeParamClause,
      f: Seq[FFuncDef]
  ) extends FNode

  // Infix expressions
  sealed trait FExpr extends FNode

  sealed trait FInfixExpr extends FExpr
  case class FAddition(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FSubtraction(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FMultiplication(lhs: FInfixExpr, rhs: FInfixExpr)
      extends FInfixExpr
  case class FDivision(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr

  case class FExprIdentifier(i: FIdentifier) extends FInfixExpr
  case class FMemberExpr(e: FInfixExpr, ids: Seq[FIdentifier] = Seq())
      extends FInfixExpr
  type Arguments = Option[Seq[FInfixExpr]]
  case class FCallExpr(
      ids: FMemberExpr,
      args: Arguments,
      a: Seq[Either[Arguments, Seq[FIdentifier]]] = Seq()
  ) extends FInfixExpr

  // Lexical tokens
  sealed trait FLiteral extends FInfixExpr
  case class FBool(b: Boolean) extends FLiteral
  case class FInt(i: Integer) extends FLiteral
  case class FFloat(f: Float) extends FLiteral
  case class FString(s: String) extends FLiteral

  case class FIdentifier(value: String) extends FNode
  case class FIndent(size: Int) extends FNode

  def checkIndents[T <: FNode](
      indents: Seq[FIndent],
      parentIndent: Option[FIndent]
  ): Boolean = {
    val firstIndentSize = indents.head.size
    val sameSize = (indent: FIndent) => indent.size == firstIndentSize
    parentIndent match {
      case Some(FIndent(parentSize)) =>
        firstIndentSize > parentSize && indents.forall(sameSize)
      case None => indents.forall(sameSize)
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

  def InputLine = rule { Program ~ EOI }
  def Program: Rule1[FNode] = rule {
    StructTypeDef | SumTypeDef | TupleTypeDef | PrimitiveTypeDef | FuncDef | Trait | TraitImpl
  }

  // Type Definitions
  def TypeParam = rule { Id ~ (wspStr("=") ~ Type).? ~> FTypeParam }
  def TypeParamClause = rule {
    "[" ~ TypeParam ~ ("," ~ TypeParam).* ~ "]" ~> (_ +: _)
  }

  // NOTE: Recursions exists here through the type arguments. The parser could
  // in theory go into an infite loop, if there is infinite amount of nesting
  // through type args.
  def Type: Rule1[FType] = rule { Id ~ TypeArgs.? ~> FType }
  def Types = rule { Type ~ ("," ~ Type).* ~> (_ +: _) }
  def TypeArgs = rule { "[" ~ Types ~ "]" }

  def Param = rule { (Id ~ ":" ~ Type) ~> FParam }
  def Params = rule { Param ~ ("," ~ Param).* ~> (_ +: _) }

  def TypeDef = rule { "type" ~ Id }
  def PrimitiveTypeDef = rule { TypeDef ~> FPrimitiveType }

  def SumTypeValueArgs = rule {
    "(" ~ (Params ~> (Left(_)) | Types ~> (Right(_))) ~ ")"
  }
  def SumTypeValue = rule {
    (NewLine ~ Indent ~ Id ~ SumTypeValueArgs.?) ~> ((i, v, t) =>
      (i, FSumTypeValue(v, t))
    )
  }
  def SumTypeDef = rule {
    (TypeDef ~ TypeParamClause.? ~ ":" ~ SumTypeValue.+) ~> ((i, t, lines) =>
      flattenIndentRule(lines)(FSumType(i, t, _))
    )
  }

  def StructTypeField = rule {
    NewLine ~ Indent ~ Param ~> ((i, p) => (i, FStructTypeField(p)))
  }
  def StructTypeDef = rule {
    (TypeDef ~ TypeParamClause.? ~ ":" ~ StructTypeField.+) ~> (
      (i, t, fields) => flattenIndentRule(fields)(FStructType(i, t, _))
    )
  }

  def TupleTypeDef = rule {
    (TypeDef ~ TypeParamClause.? ~ "(" ~ Types ~ ")") ~> FTupleType
  }

  // Func definition
  def Tail = rule { capture("tail".?) ~> ((s: String) => !s.isEmpty) }
  def FuncSig = rule {
    Tail ~ "def" ~ Id ~ TypeParamClause.? ~ "(" ~ Params.? ~ ")" ~ "->" ~ Type ~> FFuncSig
  }
  def FuncDef = rule {
    FuncSig ~ ":" ~ BlockExpr.+ ~> ((sig, exprs) => {
      flattenIndentRule(exprs)(FFuncDef(sig, _))
    })
  }

  // Trait definition + Implementations

  // TODO: Handle Func Definitions.
  def TraitFuncSig = rule { NewLine ~ Indent ~ FuncSig ~> ((_, _)) }
  def Trait = rule {
    "trait" ~ Id ~ TypeParamClause.? ~ ":" ~ TraitFuncSig.+ ~> ((i, t, funcs) =>
      flattenIndentRule(funcs)(FTrait(i, t, _))
    )
  }
  def TraitFuncDef = rule { NewLine ~ Indent ~ FuncDef ~> ((_, _)) }
  def TraitImpl = rule {
    "impl" ~ Id ~ TypeParamClause.? ~ wspStr(
      "for"
    ) ~ Id ~ TypeParamClause.? ~ ':' ~ TraitFuncDef.+ ~> ((i1, t1, i2, t2, f) =>
      flattenIndentRule(f)(FTraitImpl(i1, t1, i2, t2, _))
    )
  }

  // Expressions
  def BlockExpr = rule {
    NewLine ~ Indent ~ Expr ~> ((_, _))
  }
  def Expr: Rule1[FExpr] = InfixExpr

  def InfixExpr: Rule1[FInfixExpr] = AdditiveExpr

  def AdditiveExpr = rule {
    MultiplicativeExpr ~ (wspStr("+") ~ MultiplicativeExpr ~> FAddition |
      wspStr("-") ~ MultiplicativeExpr ~> FSubtraction).*
  }
  def MultiplicativeExpr = rule {
    UnaryExpr ~ (wspStr("*") ~ UnaryExpr ~> FMultiplication |
      wspStr("/") ~ UnaryExpr ~> FDivision).*
  }

  def UnaryExpr = rule { CallExpr | MemberExpr }

  def CallExpr = rule {
    MemberExpr ~ Arguments ~ CallExprAccessors.* ~> FCallExpr
  }
  def CallExprAccessors = rule {
    Arguments ~> (Left(_)) | DotAccessor.+ ~> (Right(_))
  }
  def Arguments = rule { "(" ~ ArgumentList.? ~ ")" }
  def ArgumentList = rule { InfixExpr ~ ("," ~ InfixExpr).* ~> (_ +: _) }

  def MemberExpr = rule { PrimaryExpr ~ DotAccessor.* ~> FMemberExpr }
  def DotAccessor = rule { '.' ~ Id }

  def PrimaryExpr = rule {
    Literal | (Id ~> FExprIdentifier) | wspStr("(") ~ InfixExpr ~ wspStr(")")
  }

  // Lexical tokens
  def Literal: Rule1[FLiteral] = rule { Bool | Float | Int | String }
  def Bool = rule { capture("true" | "false") ~> (s => FBool(s.toBoolean)) }
  def Float = rule {
    capture(DecimalInteger ~ '.' ~ CharPredicate.Digit.*) ~> (f =>
      FFloat(f.toFloat)
    )
  }
  def DecimalInteger = rule {
    '0' | (CharPredicate.Digit19 ~ CharPredicate.Digit.*)
  }
  def Int = rule { capture(CharPredicate.Digit.+) ~> (i => FInt(i.toInt)) }
  def String = rule {
    str("\"") ~ capture(CharPredicate.All.*) ~ str("\"") ~> FString
  }
  def Id = rule { capture(IdentifierPart) ~> FIdentifier }
  def IdentifierPart = rule { (AlphaNum | '_').+ }
  def NewLine = rule { ch('\n') }
  def Indent = rule { capture(Spacing.+) ~> (s => FIndent(s.size)) }
  def Spacing = rule { ch('\t') | ch(' ') }

  // Utils
  def flattenIndentRule[T <: FNode, O <: FNode](
      lines: Seq[(FIndent, T)]
  )(f: Seq[T] => O): Rule1[O] = {
    val (indents, nodes) = lines.unzip
    rule {
      test(
        checkIndents(
          indents,
          valueStack.toStream.reverse.collectFirst { case i @ FIndent(_) => i }
        )
      ) ~ push(f(nodes)) | failX(
        "correctly indendent block"
      )
    }
  }
}
