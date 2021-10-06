package code

import cats.Show
import cats.data.State
import cats.data.StateT
import cats.implicits._
import core.Bindings._
import core.Context
import core.Context._
import core.Desugar
import core.Terms._
import core.TypeChecker
import core.Types._
import parser.Info.UnknownInfo

import GrinUtils._

object Grin {
  val MainFunction = "grinMain"
  val PartialFunctionSuffix = "''"

  sealed trait Expr

  case class Value(e: String) extends Expr
  case class FunctionValue(f: String, arity: Int) extends Expr
  case class AppExpr(e: String, resultArity: Int = 0, indent: Int = 1)
      extends Expr
  case class BindExpr(
      repr: String,
      variable: String,
      value: Expr,
      indent: Int = 1
  ) extends Expr
  case class MultiLineExpr(exprs: List[BindExpr], result: Expr) extends Expr
  case class CaseClause(pattern: String, expr: Expr, indent: Int = 2)
      extends Expr
  case class CaseExpr(expr: Expr, cases: List[CaseClause], indent: Int = 1)
      extends Expr
  case class PureExpr(expr: Expr, indent: Int = 1) extends Expr
  case class DoExpr(expr: Expr, indent: Int = 1) extends Expr
  case class Abs(variable: String, expr: Expr) extends Expr

  def indent(indent: Int, instr: String) =
    List.fill(indent)(" ").mkString + instr

  def indentExpr(indent: Int, e: Expr): Expr = e match {
    case BindExpr(expr, variable, value, _) =>
      BindExpr(expr, variable, value, indent)
    case AppExpr(expr, arity, _) => AppExpr(expr, arity, indent)
    case CaseExpr(expr, cases, _) =>
      val t = cases.map(c => indentExpr(indent + 1, c)).map {
        case c: CaseClause => c
      }
      CaseExpr(indentExpr(indent, expr), t, indent)
    case CaseClause(pattern, expr, _) => CaseClause(pattern, expr, indent)
    case MultiLineExpr(exprs, expr) =>
      MultiLineExpr(
        exprs.map(l => BindExpr(l.repr, l.variable, l.value, indent)),
        indentExpr(indent, expr)
      )
    case PureExpr(expr, _) => PureExpr(indentExpr(indent, expr), indent)
    case _                 => e
  }

  implicit val showExpr: Show[Expr] =
    Show.show(_ match {
      case Value(e)             => e
      case FunctionValue(f, _)  => f
      case AppExpr(e, _, i)     => indent(i, e)
      case BindExpr(e, _, _, i) => indent(i, e)
      case MultiLineExpr(exprs, result) =>
        (exprs :+ result).map((_: Expr).show).mkString("\n")
      case CaseClause(pattern, e, i) =>
        val patternLine = indent(i, s"$pattern ->\n")
        val indentedExpr = indentExpr(i + 1, e)
        show"$patternLine$indentedExpr"
      case CaseExpr(expr, cases, i) =>
        val matchLine = indent(i, show"case $expr of")
        val caseLines = cases.map((_: Expr).show).mkString("\n")
        show"$matchLine\n$caseLines"
      case PureExpr(expr, i) =>
        expr match {
          case Value(e) => indent(i, s"pure $e")
          case MultiLineExpr(exprs, r) =>
            val pureExpr = indent(i, show"pure ($r)")
            exprs.map((_: Expr).show).mkString("\n") + "\n" + pureExpr
          case _ => expr.show
        }
      case DoExpr(expr, i) =>
        val doLine = "do\n"
        val iExpr = indentExpr(i + 1, expr)
        show"$doLine$iExpr"
      case Abs(variable, e: Abs) => show"$variable $e".strip()
      case Abs(variable, e)      => show"$variable =\n$e"
    })

  def generate(bindings: List[Bind]): String = {
    val s = for {
      values <- bindings
        .traverse(bind =>
          for {
            code <- toBinding(bind)
            id <- Context.addBinding(bind.i, bind.b)
          } yield code
        )
      (code, partialFunctions) = values.filter(!_._1.isBlank).unzip
      applyFunction <- buildApply(partialFunctions.flatten)
    } yield (code :+ applyFunction).mkString("\n\n")
    s.runEmptyA.value
  }

  def buildApply(partialFun: List[FunctionValue]): ContextState[String] =
    partialFun.isEmpty match {
      case false =>
        for {
          funVariable <- addTempVariable()
          funParameter <- addTempVariable()
          caseClauses <- partialFun.traverse(partialFunToCase(_, funParameter))
          caseExpr = CaseExpr(Value(funVariable), caseClauses.flatten)
          abs1 = Abs(funParameter, caseExpr)
          abs2 = Abs(funVariable, abs1)
        } yield show"apply $abs2"
      case true => "".pure[ContextState]
    }

  def partialFunToCase(
      partialFun: FunctionValue,
      parameter: String
  ): ContextState[List[CaseClause]] = {
    def iter(arity: Int): ContextState[List[CaseClause]] = arity match {
      case 0 => State.pure(Nil)
      case _ =>
        for {
          acc <- iter(arity - 1)
          variables <- List
            .fill(partialFun.arity - arity)("")
            .traverse(_ => addTempVariable())
          tag = pTag(arity, partialFun.f)
          vars = variables.mkString(" ").strip()
          pattern = s"($tag $vars)"
          expr = arity - 1 match {
            case 0 => AppExpr(s"${partialFun.f} $vars $parameter")
            case v =>
              val lowerArityTag = pTag(v, partialFun.f)
              AppExpr(s"pure ($lowerArityTag $vars $parameter)")
          }
        } yield CaseClause(pattern, expr) :: acc
    }
    iter(partialFun.arity)
  }

  def toBinding(binding: Bind): ContextState[(String, List[FunctionValue])] =
    binding.b match {
      case TermAbbBind(expr: TermBuiltin, _) => State.pure("", List())
      case TermAbbBind(expr, _) =>
        pureToExpr(expr).map { e =>
          {
            val bindExpr = (binding.i, expr) match {
              case ("main", _)     => show"$MainFunction $e"
              case (b, t: TermApp) => show"$b = $e"
              case (b, t)          => show"${nameBind(b)} $e"
            }
            (bindExpr, extractPartialFunctions(e))
          }
        }
      case _ => State.pure("", List())
    }

  def extractPartialFunctions(e: Expr): List[FunctionValue] = e match {
    case _: Value                                 => Nil
    case f @ FunctionValue(_, arity) if arity > 0 => List(f)
    case _: FunctionValue                         => Nil
    case _: AppExpr                               => Nil
    case BindExpr(_, _, e, _)                     => extractPartialFunctions(e)
    case DoExpr(e, _)                             => extractPartialFunctions(e)
    case CaseClause(_, e, _)                      => extractPartialFunctions(e)
    case PureExpr(e, _)                           => extractPartialFunctions(e)
    case Abs(_, e)                                => extractPartialFunctions(e)
    case CaseExpr(e, cases, _) =>
      val casesPartialFunctions = cases.map(extractPartialFunctions(_)).flatten
      extractPartialFunctions(e) :++ casesPartialFunctions
    case MultiLineExpr(exprs, r) =>
      exprs.map(extractPartialFunctions(_)).flatten :++ extractPartialFunctions(
        r
      )

  }

  def nameBind(binding: String): String = binding match {
    case b if b.startsWith(Desugar.MethodNamePrefix) =>
      methodToName(b)
    case b if b.startsWith(Desugar.RecordConstrPrefix) =>
      recordConstrToName(b)
    case n => n
  }

  def pureToExpr(expr: Term): ContextState[Expr] =
    Context.run(toExpr(expr))

  def toExpr(expr: Term): ContextState[Expr] =
    expr match {
      case TermBuiltin(_)       => StateT.pure(Value(""))
      case TermAscribe(_, t, _) => toExpr(t)
      case TermFix(_, body)     => pureToExpr(body)
      case TermAbs(_, variable, variableType, abs: TermAbs, _) =>
        for {
          variable1 <- includeFunctionSuffix(variable, variableType)
          variable2 <- Context.addBinding(variable1, VarBind(variableType))
          a <- pureToExpr(abs: Term)
        } yield Abs(toParamVariable(variable2), a)
      case TermAbs(_, variable, variableType, body, _) =>
        for {
          variable1 <- includeFunctionSuffix(variable, variableType)
          variable2 <- Context.addBinding(variable1, VarBind(variableType))
          b <- pureToExpr(body)
        } yield Abs(toParamVariable(variable2), b)
      case TermApp(_, TermFold(_, ty), r: TermRecord) =>
        for {
          typeName <- getNameFromType(ty)
          recordExpr <- pureToExpr(r)
          (prepExprs, parameters) <- prepParameters(recordExpr)
          constr = show"pure (${cTag(typeName)} $parameters)"
        } yield MultiLineExpr(prepExprs, AppExpr(constr))
      case TermApp(_, value, svalue) =>
        for {
          v <- pureToExpr(value)
          sval <- pureToExpr(svalue)
          (prepExprs1, result) <- getResult(v)
          (prepExprs2, parameter) <- prepParameters(sval)
          prepExprs = prepExprs1 :++ prepExprs2
          app = result match {
            case FunctionValue(f, arity) if f.contains(PartialFunctionSuffix) =>
              AppExpr(show"apply $result $parameter".strip(), arity - 1)
            case _ =>
              AppExpr(show"$result $parameter".strip())
          }
        } yield MultiLineExpr(prepExprs, app)
      case TermLet(_, variable, t1, t2) =>
        for {
          letValue <- pureToExpr(t1)
          (prepExprs, letExpr) <- getResult(letValue)
          fVar <- typeCheck(t1).flatMap(ty =>
            Context.addBinding(variable, VarBind(ty))
          )
          expr <- pureToExpr(t2)
        } yield MultiLineExpr(
          prepExprs :++ List(
            BindExpr(show"$fVar <- $letExpr".strip(), fVar, letExpr)
          ),
          expr
        )
      case TermTag(_, tag, TermUnit(_), _) =>
        State.pure(Value(s"(${cTag(tag)})"))
      case TermTag(_, tag, term, _) =>
        for {
          params <- toExpr(term)
          (prepExprs, values) <- prepParameters(params)
          constr = show"(${cTag(tag)} $values)"
        } yield MultiLineExpr(prepExprs, Value(constr))
      case TermRecord(_, fields) =>
        fields
          .traverse { case (_, term) =>
            for {
              e <- toExpr(term)
              isNode <- isNodeValue(term)
              v: Tuple2[Option[BindExpr], Expr] <- isNode match {
                case true =>
                  addTempVariable().map(p =>
                    (
                      Some(BindExpr(show"$p <- store $e", p, e)),
                      Value(p): Expr
                    )
                  )
                case false =>
                  State.pure[Context, Tuple2[Option[BindExpr], Expr]]((None, e))
              }
            } yield v
          }
          .map(exprs => {
            val prepExprs = exprs.map(_._1).flatten
            MultiLineExpr(
              prepExprs,
              Value(exprs.map { case (_, p) => p.show }.mkString(" "))
            )
          })
      case TermProj(_, t, label) =>
        for {
          e <- pureToExpr(t)
          ty <- typeCheck(t)
          typeName <- getNameFromType(ty)
          tyS <- TypeChecker.simplifyType(ty)
          (variables, labelVariable) <- tyS match {
            case TypeRec(_, _, _, TypeRecord(_, fields)) =>
              fields
                .traverse(_ => addTempVariable())
                .map(v =>
                  (
                    v.mkString(" "),
                    // TODO: We probably need to fetch a node value here.
                    v.get(fields.indexWhere { case (f, _) => f == label }).get
                  )
                )
          }
          doExpr: Expr = DoExpr(
            CaseExpr(
              e,
              List(
                CaseClause(
                  s"(${cTag(typeName)} $variables)",
                  PureExpr(Value(labelVariable))
                )
              )
            )
          )
          bindVar <- addTempVariable()
        } yield MultiLineExpr(
          List(BindExpr(show"$bindVar <- $doExpr", bindVar, doExpr)),
          Value(bindVar)
        )
      case TermMatch(_, e, patterns) =>
        for {
          ty1 <- typeCheck(e)
          exprType <- TypeChecker.unfoldType(ty1)
          t <- pureToExpr(e)
          p <- patterns.traverse { case (p, e) =>
            Context.run(toCaseClause(p, e, exprType))
          }
        } yield CaseExpr(t, p)
      case TermMethodProj(_, t, method) =>
        for {
          tyT1 <- typeCheck(t)
          tyT1S <- TypeChecker.simplifyType(tyT1)
          typeName <- getNameFromType(tyT1)
          f = methodToName(Desugar.toMethodId(method, typeName))
        } yield Value(s"$f")
      case TermFold(_, _)   => StateT.pure(Value("pure "))
      case TermInt(_, i)    => StateT.pure(Value(i.toString))
      case TermFloat(_, f)  => StateT.pure(Value(f.toString))
      case TermString(_, s) => StateT.pure(Value(s"""#"$s""""))
      case TermTrue(_)      => StateT.pure(Value("#True"))
      case TermFalse(_)     => StateT.pure(Value("#False"))
      case TermUnit(_)      => State.pure(Value("()"))
      case TermVar(_, idx, _) =>
        for {
          variable <- toVariable(idx)
          binding <- toContextState(Context.getBinding(UnknownInfo, idx))
          expr <- (variable, binding) match {
            case (v, VarBind(ty)) =>
              isFunctionType(ty).map(_ match {
                case false => Value(v)
                case true  => FunctionValue(v, getFunctionArity(ty))
              })
            case (v, TermAbbBind(t, Some(ty))) =>
              FunctionValue(v, getFunctionArity(ty)).pure[ContextState]
            case (v, _) => Value(v).pure[ContextState]
          }
        } yield expr
    }

  def toCaseClause(
      p: Pattern,
      e: Term,
      matchExprType: Type
  ): ContextState[CaseClause] = p match {
    case PatternNode(_, node, vars) =>
      for {
        (_, bindVariables) <- toContextState(
          TypeChecker.typeOfPattern(p, matchExprType, dummyType)
        )
        cpat = s"(${cTag(node)} ${bindVariables.mkString(" ")})"
        caseClause <- buildCaseClause(cpat, e)
      } yield caseClause
    case PatternDefault(_) => buildCaseClause("#default", e)
    case t: Term           => pureToExpr(t).flatMap(cpat => buildCaseClause(cpat.show, e))
  }

  def buildCaseClause(cpat: String, t: Term): ContextState[CaseClause] = for {
    caseExpr <- pureToExpr(t)
    (prepExprs, parameter) <- prepParameters(caseExpr)
  } yield CaseClause(
    cpat,
    MultiLineExpr(prepExprs, PureExpr(Value(parameter)))
  )

  def prepParameters(
      expr: Expr
  ): ContextState[Tuple2[List[BindExpr], String]] =
    expr match {
      case b @ BindExpr(e, v, _, _) =>
        ((List(b), v)).pure[ContextState]
      case MultiLineExpr(exprs, r) =>
        prepParameters(r).map { case (prepExprs, result) =>
          (exprs :++ prepExprs, result)
        }
      case c @ AppExpr(e, _, i) =>
        addTempVariable().flatMap(p =>
          prepParameters(BindExpr(s"$p <- $e", p, c, i))
        )
      case v @ FunctionValue(f, 0) =>
        addTempVariable().flatMap(p =>
          prepParameters(BindExpr(s"$p <- $f", p, v))
        )
      case v @ FunctionValue(f, arity) if !f.contains(PartialFunctionSuffix) =>
        addTempVariable().flatMap(p =>
          prepParameters(BindExpr(s"$p <- pure (${pTag(arity, f)})", p, v))
        )
      case FunctionValue(f, _) => State.pure(List(), f)
      case v @ Value(s) if s.contains("'") =>
        addTempVariable().flatMap(p =>
          prepParameters(BindExpr(s"$p <- fetch $s", p, v))
        )
      case Value(v) => State.pure(List(), v)
    }

  def getResult(
      expr: Expr
  ): ContextState[Tuple2[List[BindExpr], Expr]] = expr match {
    case l: BindExpr => State.pure(List(l), Value(l.variable))
    case MultiLineExpr(exprs, r) =>
      getResult(r).map { case (prepExprs, v) =>
        (exprs :++ prepExprs, v)
      }
    case i: Value         => State.pure(List(), i)
    case f: FunctionValue => State.pure(List(), f)
    case a @ AppExpr(e, arity, _) if arity >= 1 =>
      addTempVariable().map(p => {
        val f = toPartialFunVariable(p)
        (
          List(BindExpr(show"$f <- $e", f, a)),
          FunctionValue(s"$f", arity)
        )
      })
    case e: AppExpr  => State.pure(List(), e)
    case c: CaseExpr => State.pure(List(), DoExpr(c, c.indent))
  }

  def toParamVariable(v: String): String = v match {
    case WildcardName                                            => ""
    case s if s.startsWith(Desugar.RecursiveFunctionParamPrefix) => ""
    case _                                                       => v
  }

  def includeFunctionSuffix(v: String, ty: Type): ContextState[String] =
    isFunctionType(ty).map(_ match {
      case true
          if !v.isBlank && !v
            .startsWith(Desugar.RecursiveFunctionParamPrefix) =>
        toPartialFunVariable(v)
      case _ => v
    })

  def toPartialFunVariable(p: String): String = s"$p$PartialFunctionSuffix"

  def toVariable(idx: Integer): ContextState[String] =
    getNameFromIndex(idx).map(_ match {
      case "&add"       => "_prim_int_add"
      case "print"      => "_prim_string_print"
      case "int_to_str" => "_prim_int_str"
      case v if v.startsWith(Desugar.RecursiveFunctionParamPrefix) =>
        v.stripPrefix(Desugar.RecursiveFunctionParamPrefix)
      case v if v.startsWith(Desugar.MethodNamePrefix) => methodToName(v)
      case v if v.startsWith(Desugar.RecordConstrPrefix) =>
        recordConstrToName(v)
      case s => s
    })

  def methodToName(n: String): String =
    s"${n.filter(_.isLetterOrDigit)}'"

  def recordConstrToName(n: String): String =
    n.stripPrefix(Desugar.RecordConstrPrefix)

  def addTempVariable(name: String = "p"): ContextState[String] =
    Context.addBinding(name, TempVarBind)
}
