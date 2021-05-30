package parser

import org.parboiled2._
import scala.util.Either

object FuseExpressionParser {
  import FuseLexicalParser._
  import FuseTypesParser._

  sealed trait FExpr

  case class FLetExpr(i: FIdentifier, t: Option[FType], e: Seq[FExpr])
      extends FExpr

  case class FBinding(i: FIdentifier, t: Option[FType] = None)
  case class FAbs(params: Seq[FBinding], t: Option[FType], e: Seq[FExpr])
      extends FExpr

  sealed trait FPattern
  case class FIdentifierPattern(value: String, p: Option[FPattern] = None)
      extends FPattern
  object FWildCardPattern extends FPattern
  case class FTuplePattern(s: Seq[FPattern]) extends FPattern
  case class FVariantOrRecordPattern(i: FIdentifier, s: Seq[FPattern])
      extends FPattern
  case class FCase(p: Seq[FPattern], guard: Option[FInfixExpr], e: Seq[FExpr])
  case class FMatch(e: FInfixExpr, c: Seq[FCase]) extends FExpr

  sealed trait FInfixExpr extends FExpr
  case class FAddition(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FSubtraction(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FMultiplication(lhs: FInfixExpr, rhs: FInfixExpr)
      extends FInfixExpr
  case class FDivision(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FModulo(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FEquality(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FNotEquality(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FAnd(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FOr(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FLessThan(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FLessThanEqual(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FGreaterThan(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FGreaterThanEqual(lhs: FInfixExpr, rhs: FInfixExpr)
      extends FInfixExpr

  case class FVar(value: String) extends FInfixExpr
  case class FProj(e: FInfixExpr, ids: Seq[FVar]) extends FInfixExpr
  type FArguments = Option[Seq[FExpr]]
  type FTypeArguments = Option[Seq[FType]]
  // NOTE: This is the actual application, but the arguments can be optional
  // indicating the abstraction accepts a unit.
  case class FApp(
      e: FExpr,
      typeArguments: FTypeArguments = None,
      args: Seq[FArguments]
  ) extends FInfixExpr

  // Literals
  sealed trait FLiteral extends FInfixExpr with FPattern
  case class FBool(b: Boolean) extends FLiteral
  case class FInt(i: Integer) extends FLiteral
  case class FFloat(f: Float) extends FLiteral
  case class FString(s: String) extends FLiteral
}

class FuseExpressionParser(val input: ParserInput) extends FuseTypesParser {
  import FuseExpressionParser._
  import FuseLexicalParser._

  def BlockExpr = oneOrMoreWithIndent(ExprVal)
  val ExprVal = () => Expr
  def Expr: Rule1[FExpr] = rule {
    LetExpr | LambdaExpr | MatchExpr | InfixExpr
  }

  def InlineExpr: Rule1[Seq[FExpr]] = {
    def InlineBlockExpr = rule {
      '{' ~ BlockExpr ~ NewLine ~ Indent ~ '}' ~> (
        (e: Seq[FExpr], i: FIndent) => {
          validateIndents(Seq(i)) ~ push(e) | failX(
            "correctly indendent inline block"
          )
        }
      )
    }
    rule { LambdaExpr ~> (Seq(_)) | InfixExpr ~> (Seq(_)) | InlineBlockExpr }
  }
  def LetExpr = rule {
    "let" ~ Id ~ (":" ~ Type).? ~ wspStr("=") ~ InlineExpr ~> FLetExpr
  }
  def LambdaExpr = {
    def Binding = rule { Id ~ (":" ~ Type).? ~> FBinding }
    def Bindings = rule { '(' ~ Binding.*(",") ~ ')' }
    def ReturnType = rule { wspStr("->") ~ Type }
    rule {
      (Bindings | Id ~> (i => Seq(FBinding(i)))) ~ ReturnType.? ~ wspStr("=>") ~
        InlineExpr ~> FAbs
    }
  }

  def MatchExpr = {
    def Pattern: Rule1[FPattern] = rule {
      Id ~ "(" ~ Patterns ~ ")" ~> FVariantOrRecordPattern |
        "(" ~ Patterns ~ ")" ~> FTuplePattern |
        Literal |
        capture("_") ~> (_ => FWildCardPattern) |
        capture(IdentifierPart) ~ (wspStr("@") ~ Pattern).? ~>
        FIdentifierPattern
    }
    def Patterns = rule {
      Pattern.*(",")
    }
    def Guard = rule { wspStr("if") ~ InfixExpr }
    def ArmPatterns = rule {
      Pattern.+("|")
    }
    def Case = () =>
      rule {
        ArmPatterns ~ Guard.? ~ wspStr("=>") ~ InlineExpr ~> FCase
      }
    rule {
      "match" ~ InfixExpr ~ ":" ~ oneOrMoreWithIndent(Case) ~> FMatch
    }
  }

  def InfixExpr: Rule1[FInfixExpr] = {
    def PrimaryExpr = rule {
      Literal | ExprId | wspStr("(") ~ InfixExpr ~ wspStr(")")
    }

    def CallExpr = {
      def ArgumentExpr = rule { LambdaExpr | InfixExpr }
      def ArgumentList = rule { ArgumentExpr.+(",") }
      def Arguments = rule { "(" ~ ArgumentList.? ~ ")" }
      def TypeArguments = rule { "[" ~ Type.+(",") ~ "]" }
      rule {
        (Proj | PrimaryExpr) ~ TypeArguments.? ~ Arguments.+ ~> FApp
      }
    }

    def Proj = rule { PrimaryExpr ~ ('.' ~ ExprId).+ ~> FProj }

    def UnaryExpr = rule { CallExpr | Proj | PrimaryExpr }

    def MultiplicativeExpr = rule {
      UnaryExpr ~ (wspStr("*") ~ UnaryExpr ~> FMultiplication |
        wspStr("/") ~ UnaryExpr ~> FDivision |
        wspStr("%") ~ UnaryExpr ~> FModulo).*
    }
    def AdditiveExpr = rule {
      MultiplicativeExpr ~ (wspStr("+") ~ MultiplicativeExpr ~> FAddition |
        wspStr("-") ~ MultiplicativeExpr ~> FSubtraction).*
    }
    def RelationExpr = rule {
      AdditiveExpr ~ (wspStr("<=") ~ AdditiveExpr ~> FLessThanEqual |
        wspStr(">=") ~ AdditiveExpr ~> FGreaterThanEqual |
        wspStr("<") ~ AdditiveExpr ~> FLessThan |
        wspStr(">") ~ AdditiveExpr ~> FGreaterThan).*

    }
    def EqualityExpr = rule {
      RelationExpr ~ (wspStr("==") ~ RelationExpr ~> FEquality |
        wspStr("!=") ~ RelationExpr ~> FNotEquality).*
    }
    def LogicalAnd = rule {
      EqualityExpr ~ (wspStr("&&") ~ EqualityExpr ~> FAnd).*
    }
    def LogicalOr = rule {
      LogicalAnd ~ (wspStr("||") ~ LogicalAnd ~> FOr).*
    }

    LogicalOr
  }

  // Literals
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

  private def ExprId = rule { capture(IdentifierPart) ~> FVar }
}
