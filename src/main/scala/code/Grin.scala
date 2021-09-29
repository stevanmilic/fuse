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

  val dummyType = TypeUnit(UnknownInfo)

  sealed trait Expr

  case class InlineExpr(e: String) extends Expr
  case class LineExpr(e: String, indent: Int = 1) extends Expr
  case class MultiLineExpr(exprs: List[LineExpr], result: Expr) extends Expr
  case class CaseClause(pattern: String, expr: Expr, indent: Int = 2)
      extends Expr
  case class CaseExpr(expr: Expr, cases: List[CaseClause], indent: Int = 1)
      extends Expr
  case class PureExpr(expr: Expr) extends Expr

  def indent(indent: Int, instr: String) =
    List.fill(indent)(" ").mkString + instr

  implicit val showExpr: Show[Expr] =
    Show.show(_ match {
      case InlineExpr(e)  => e
      case LineExpr(e, i) => indent(i, e)
      case MultiLineExpr(exprs, result) =>
        (exprs :+ result).map((_: Expr).show).mkString("\n")
      case CaseClause(pattern, expr, i) =>
        indent(i, show"$pattern -> $expr")
      case CaseExpr(expr, cases, i) =>
        val matchLine = indent(i, show"case $expr of")
        val caseLines = cases.map((_: Expr).show).mkString("\n")
        s"$matchLine\n$caseLines"
      case PureExpr(expr) =>
        expr match {
          case InlineExpr(e) => s"pure $e"
          case MultiLineExpr(exprs, r) =>
            exprs.map((_: Expr).show).mkString("\n") + show"\npure ($r)"
          case _ => expr.show
        }
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
          case ("main", _)     => show"$MainFunction $e"
          case (b, t: TermApp) => show"$b = $e"
          case (b, t)          => show"$b $e"
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
    case TermTag(_, tag, TermUnit(_), _) =>
      State.pure(InlineExpr(s"(${cTag(tag)})"))
    case TermTag(_, tag, TermRecord(_, fields), _) =>
      fields
        .traverse { case (_, term) =>
          for {
            e <- toExpr(term)
            isNode <- isNodeValue(term)
            v: Tuple2[Option[LineExpr], Expr] <- isNode match {
              case true =>
                pickFreshName("p")
                  .map(p =>
                    (
                      Some(LineExpr(show"$p <- store $e")),
                      InlineExpr(p): Expr
                    )
                  )
              case false =>
                State.pure[Context, Tuple2[Option[LineExpr], Expr]]((None, e))
            }
          } yield v
        }
        .map(exprs => {
          val prepExprs = exprs.map(_._1).flatten
          val parameters = exprs.map { case (_, p) => p.show }.mkString(" ")
          val constr = s"(${cTag(tag)} $parameters)"
          if (prepExprs.isEmpty) InlineExpr(constr)
          else
            MultiLineExpr(prepExprs, InlineExpr(constr))

        })
    case TermMatch(_, e, patterns) =>
      for {
        ty1 <- typeCheck(e)
        exprType <- TypeChecker.unfoldType(ty1)
        t <- toExpr(e)
        p <- patterns.traverse { case (p, e) => toCaseClause(p, e, exprType) }
      } yield CaseExpr(t, p)
    case TermFold(_, _)     => StateT.pure(InlineExpr("pure"))
    case TermInt(_, i)      => StateT.pure(InlineExpr(i.toString))
    case TermFloat(_, f)    => StateT.pure(InlineExpr(f.toString))
    case TermString(_, s)   => StateT.pure(InlineExpr(s"""#"$s""""))
    case TermTrue(_)        => StateT.pure(InlineExpr("#True"))
    case TermFalse(_)       => StateT.pure(InlineExpr("#False"))
    case TermVar(_, idx, _) => toVariable(idx).map(InlineExpr(_))
  }

  def toCaseClause(
      p: Pattern,
      e: Term,
      matchExprType: Type
  ): ContextState[CaseClause] = p match {
    case PatternNode(_, node, vars) =>
      for {
        _ <- toContextState(
          TypeChecker.typeOfPattern(p, matchExprType, dummyType)
        )
        caseExpr <- toExpr(e)
        cpat = s"(${cTag(node)} ${vars.mkString(" ")})"
      } yield CaseClause(cpat, PureExpr(caseExpr))
    case t: Term =>
      toExpr(t).map2(toExpr(e)) { case (p, e) =>
        CaseClause(p.show, PureExpr(e))
      }
  }

  def prepParameters(expr: Expr): ContextState[Tuple2[List[LineExpr], String]] =
    expr match {
      case LineExpr(e, n) =>
        pickFreshName("p").map(p => (List(LineExpr(s"$p <- $e", n)), p))
      case MultiLineExpr(exprs, r) => for {
        (prepExprs, result) <- prepParameters(r)
      } yield (exprs :++ prepExprs, result)
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

  def cTag(tag: String) = s"C$tag"

  def isNodeValue(t: Term): ContextState[Boolean] = typeCheck(t)
    .flatMap(TypeChecker.simplifyType(_))
    .map(_ match {
      case _: TypeRec => true
      case v => false
    })

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

  def typeCheck(term: Term): ContextState[Type] =
    toContextState(TypeChecker.pureTypeOf(term))

  def toContextState[T](stateEither: StateEither[T]): ContextState[T] =
    stateEither.value.map(v => v.toOption.get)
}
