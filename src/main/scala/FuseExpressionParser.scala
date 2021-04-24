package fuse

import org.parboiled2._
import scala.util.Either

object FuseExpressionParser {
  import FuseLexicalParser._
  import FuseTypesParser._

  sealed trait FExpr

  case class FLetExpr(i: FIdentifier, t: Option[FType], e: Seq[FExpr])
      extends FExpr

  case class FLambdaBinding(i: FIdentifier, t: Option[FType] = None)
  case class FLambdaExpr(params: Seq[FLambdaBinding], e: Seq[FExpr])
      extends FExpr

  sealed trait FPattern
  case class FIdentifierPattern(value: String, p: Option[FPattern] = None)
      extends FPattern
  object FWildCardPattern extends FPattern
  case class FTuplePattern(s: Seq[FPattern]) extends FPattern
  case class FSumStructPattern(i: FIdentifier, s: Seq[FPattern])
      extends FPattern
  case class FCase(p: Seq[FPattern], guard: Option[FInfixExpr], e: Seq[FExpr])
  case class FMatchExpr(e: FInfixExpr, c: Seq[FCase]) extends FExpr

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

  case class FExprIdentifier(value: String) extends FInfixExpr
  case class FMemberExpr(e: FInfixExpr, ids: Seq[FExprIdentifier])
      extends FInfixExpr
  type FArguments = Option[Seq[FExpr]]
  case class FCallExpr(
      e: FExpr,
      args: FArguments,
      a: Seq[Either[FArguments, Seq[FExprIdentifier]]] = Seq()
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
    def Binding = rule { Id ~ (":" ~ Type).? ~> FLambdaBinding }
    def Bindings = rule { '(' ~ Binding.*(",") ~ ')' }
    rule {
      (Bindings | Id ~> (i => Seq(FLambdaBinding(i)))) ~ wspStr("=>") ~
        InlineExpr ~> FLambdaExpr
    }
  }

  def MatchExpr = {
    def Pattern: Rule1[FPattern] = rule {
      Id ~ "(" ~ Patterns ~ ")" ~> FSumStructPattern |
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
      "match" ~ InfixExpr ~ ":" ~ oneOrMoreWithIndent(Case) ~> FMatchExpr
    }
  }

  def InfixExpr: Rule1[FInfixExpr] = {
    def PrimaryExpr = rule {
      Literal | ExprId | wspStr("(") ~ InfixExpr ~ wspStr(")")
    }

    def DotAccessor = rule { '.' ~ ExprId }

    def CallExpr = {
      def ArgumentExpr = rule { LambdaExpr | InfixExpr }
      def ArgumentList = rule { ArgumentExpr.+(",") }
      def Arguments = { rule { "(" ~ ArgumentList.? ~ ")" } }
      def CallExprAccessors = rule {
        Arguments ~> (Left(_)) | DotAccessor.+ ~> (Right(_))
      }
      rule {
        (MemberExpr | PrimaryExpr) ~ Arguments ~ CallExprAccessors.* ~> FCallExpr
      }
    }

    def MemberExpr = rule { PrimaryExpr ~ DotAccessor.+ ~> FMemberExpr }

    def UnaryExpr = rule { CallExpr | MemberExpr | PrimaryExpr }

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

  private def ExprId = rule { capture(IdentifierPart) ~> FExprIdentifier }
}
