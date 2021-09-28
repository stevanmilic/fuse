package code

import cats.implicits._
import cats.data.{StateT, State}
import core.Bindings._
import core.Terms._
import core.Types._
import core.Context
import core.Context._
import core.TypeChecker
import cats.Show
import parser.Info.UnknownInfo

object Grin {
  val MainFunction = "grinMain"

  sealed trait Expr

  case class InlineExpr(e: String) extends Expr
  case class LineExpr(e: String, nested: Int = 1) extends Expr
  case class MultiLineExpr(exprs: List[LineExpr], result: LineExpr) extends Expr

  implicit val showExpr: Show[Expr] =
    Show.show(_ match {
      case InlineExpr(e)  => e
      case LineExpr(e, n) => List.fill(n)(" ").mkString + e
      case MultiLineExpr(exprs, result) =>
        (exprs :+ result).map(e => (e: Expr).show).mkString("\n")
    })

  def generate(bindings: List[Bind]): String =
    bindings
      .traverse(bind =>
        for {
          code <- toBinding(bind)
          id <- Context.addBinding(bind.i, bind.b)
        } yield code
      )
      .runEmptyA
      .value
      .filter(!_.isBlank)
      .mkString("\n\n")

  def toBinding(binding: Bind): ContextState[String] = binding.b match {
    case TermAbbBind(expr: TermBuiltin, _) => "".pure[ContextState]
    case TermAbbBind(expr, _) =>
      pureToExpr(expr).map { e =>
        (binding.i, expr) match {
          case (TypeChecker.MainFunction, _) => show"$MainFunction $e"
          case (b, t: TermAbs)               => show"$b $e"
          case (b, t)                        => show"$b = $e"
        }
      }
    case _ => "".pure[ContextState]
  }

  def pureToExpr(expr: Term): ContextState[Expr] =
    Context.run(toExpr(expr))

  def toExpr(expr: Term): ContextState[Expr] = expr match {
    case TermBuiltin(_)   => StateT.pure(InlineExpr(""))
    case TermFix(_, body) => pureToExpr(body)
    case TermAbs(_, variable, variableType, abs: TermAbs, _) =>
      for {
        _ <- Context.addBinding(variable, VarBind(variableType))
        a <- pureToExpr(abs: Term)
      } yield InlineExpr(show"${toParamVariable(variable)} $a".strip())
    case TermAbs(_, variable, variableType, body, _) =>
      for {
        _ <- Context.addBinding(variable, VarBind(variableType))
        b <- pureToExpr(body)
      } yield InlineExpr(show"${toParamVariable(variable)} =\n$b".strip())
    case TermApp(_, value, svalue) =>
      for {
        v <- toExpr(value)
        sval <- toExpr(svalue)
        (prepExprs1, result) = getResult(v)
        (prepExprs2, parameter) <- prepParameters(sval)
        prepExprs = prepExprs1 :++ prepExprs2
        app = LineExpr(show"$result $parameter".strip())
      } yield
        if (prepExprs.isEmpty) app
        else MultiLineExpr(prepExprs, app)
    case TermLet(_, variable, t1, t2) =>
      for {
        letValue <- pureToExpr(t1)
        (prepExprs, letExpr) = getResult(letValue)
        _ <- typeCheck(t1).flatMap(ty =>
          Context.addBinding(variable, VarBind(ty))
        )
        expr <- toExpr(t2)
      } yield MultiLineExpr(
        prepExprs :++ List(LineExpr(show"$variable <- $letExpr".strip())),
        LineExpr(show"$expr".strip())
      )
    case TermTag(_, tag, TermUnit(_), _) => State.pure(InlineExpr(s"(C$tag)"))
    case TermTag(_, tag, TermRecord(_, fields), _) =>
      fields
        .traverse { case (_, term) => toExpr(term) }
        .map(exprs =>
          InlineExpr(s"(C$tag ${exprs.map(e => e.show).mkString(" ")})")
        )
    case TermFold(_, _)     => StateT.pure(InlineExpr("pure"))
    case TermInt(_, i)      => StateT.pure(InlineExpr(i.toString))
    case TermFloat(_, f)    => StateT.pure(InlineExpr(f.toString))
    case TermString(_, s)   => StateT.pure(InlineExpr(s"""#"$s""""))
    case TermTrue(_)        => StateT.pure(InlineExpr("#True"))
    case TermFalse(_)       => StateT.pure(InlineExpr("#False"))
    case TermVar(_, idx, _) => toVariable(idx).map(InlineExpr(_))
  }

  def typeCheck(term: Term): ContextState[Type] =
    TypeChecker.pureTypeOf(term).value.map(v => v.toOption.get)

  def prepParameters(expr: Expr): ContextState[Tuple2[List[LineExpr], String]] =
    expr match {
      case LineExpr(e, n) =>
        pickFreshName("p").map(p => (List(LineExpr(s"$p <- $e", n)), p))
      case MultiLineExpr(exprs, r) =>
        pickFreshName("p").map(p => (exprs :+ LineExpr(show"$p <- $r"), p))
      case InlineExpr(v) => StateT.pure(List(), v)
    }

  def getResult(expr: Expr): Tuple2[List[LineExpr], Expr] = expr match {
    case l: LineExpr             => (List(), l)
    case MultiLineExpr(exprs, r) => (exprs, r)
    case i: InlineExpr           => (List(), i)
  }

  def toParamVariable(v: String): String = v match {
    case WildcardName           => ""
    case s if s.startsWith("^") => ""
    case _                      => v
  }

  def toVariable(idx: Integer): ContextState[String] =
    State
      .inspect { (ctx: Context) =>
        Context
          .indexToName(
            ctx.filter {
              case (_, NameBind) => false
              case _             => true
            },
            idx
          )
          .get
      }
      .map(_ match {
        case "&add"       => "_prim_int_add"
        case "print"      => "_prim_string_print"
        case "int_to_str" => "_prim_int_str"
        case s            => s
      })
}
