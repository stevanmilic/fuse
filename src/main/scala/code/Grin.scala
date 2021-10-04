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
import core.Desugar
import parser.Info.UnknownInfo

object Grin {
  val MainFunction = "grinMain"

  val dummyType = TypeUnit(UnknownInfo)

  sealed trait Expr

  case class Value(e: String) extends Expr
  case class ComplexExpr(e: String, indent: Int = 1) extends Expr
  case class BindExpr(e: String, v: String, indent: Int = 1) extends Expr
  case class MultiLineExpr(exprs: List[BindExpr], result: Expr) extends Expr
  case class CaseClause(pattern: String, expr: Expr, indent: Int = 2)
      extends Expr
  case class CaseExpr(expr: Expr, cases: List[CaseClause], indent: Int = 1)
      extends Expr
  case class PureExpr(expr: Expr, indent: Int = 1) extends Expr
  case class DoExpr(expr: Expr, indent: Int = 1) extends Expr

  def indent(indent: Int, instr: String) =
    List.fill(indent)(" ").mkString + instr

  def indentExpr(indent: Int, e: Expr): Expr = e match {
    case BindExpr(expr, v, _) => BindExpr(expr, v, indent)
    case ComplexExpr(expr, _) => ComplexExpr(expr, indent)
    case CaseExpr(expr, cases, _) =>
      val t = cases.map(c => indentExpr(indent + 1, c)).map {
        case c: CaseClause => c
      }
      CaseExpr(indentExpr(indent, expr), t, indent)
    case CaseClause(pattern, expr, _) => CaseClause(pattern, expr, indent)
    case MultiLineExpr(exprs, expr) =>
      MultiLineExpr(
        exprs.map(l => BindExpr(l.e, l.v, indent)),
        indentExpr(indent, expr)
      )
    case PureExpr(expr, _) => PureExpr(indentExpr(indent, expr), indent)
    case _                 => e
  }

  implicit val showExpr: Show[Expr] =
    Show.show(_ match {
      case Value(e)          => e
      case ComplexExpr(e, i) => indent(i, e)
      case BindExpr(e, _, i) => indent(i, e)
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
          case (b, t)          => show"${nameBind(b)} $e"
        }
      }
    case _ => "".pure[ContextState]
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
          fVar <- Context.addBinding(variable, VarBind(variableType))
          a <- pureToExpr(abs: Term)
        } yield Value(show"${toParamVariable(fVar)} $a".strip())
      case TermAbs(_, variable, variableType, body, _) =>
        for {
          fVar <- Context.addBinding(variable, VarBind(variableType))
          b <- pureToExpr(body)
        } yield Value(show"${toParamVariable(fVar)} =\n$b".strip())
      case TermApp(_, TermFold(_, ty), r: TermRecord) =>
        for {
          typeName <- getNameFromType(ty)
          recordExpr <- toExpr(r)
          (prepExprs, parameters) <- prepParameters(recordExpr)
          constr = show"pure (${cTag(typeName)} $parameters)"
        } yield MultiLineExpr(prepExprs, ComplexExpr(constr))
      case TermApp(_, value, svalue) =>
        for {
          v <- toExpr(value)
          sval <- toExpr(svalue)
          (prepExprs1, result) = getResult(v)
          (prepExprs2, parameter) <- prepParameters(sval)
          prepExprs = prepExprs1 :++ prepExprs2
          app = ComplexExpr(show"$result $parameter".strip())
        } yield MultiLineExpr(prepExprs, app)
      case TermLet(_, variable, t1, t2) =>
        for {
          letValue <- pureToExpr(t1)
          (prepExprs, letExpr) = getResult(letValue)
          fVar <- typeCheck(t1).flatMap(ty =>
            Context.addBinding(variable, VarBind(ty))
          )
          expr <- toExpr(t2)
        } yield MultiLineExpr(
          prepExprs :++ List(BindExpr(show"$fVar <- $letExpr".strip(), fVar)),
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
                      Some(BindExpr(show"$p <- store $e", p)),
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
          e <- toExpr(t)
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
          List(BindExpr(show"$bindVar <- $doExpr", bindVar)),
          Value(bindVar)
        )
      case TermMatch(_, e, patterns) =>
        for {
          ty1 <- typeCheck(e)
          exprType <- TypeChecker.unfoldType(ty1)
          t <- toExpr(e)
          p <- patterns.traverse { case (p, e) => toCaseClause(p, e, exprType) }
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
        toVariable(idx).map2(
          toContextState(Context.getBinding(UnknownInfo, idx))
        ) {
          case (v, VarBind(b)) => Value(v)
          case (v, _)          => ComplexExpr(v)
        }
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
        caseClause <- toCaseClause(cpat, e)
      } yield caseClause
    case PatternDefault(_) => toCaseClause("#default", e)
    case t: Term           => toExpr(t).flatMap(cpat => toCaseClause(cpat.show, e))
  }

  def toCaseClause(cpat: String, t: Term): ContextState[CaseClause] = for {
    caseExpr <- toExpr(t)
    (prepExprs, parameter) <- prepParameters(caseExpr)
  } yield CaseClause(
    cpat,
    MultiLineExpr(prepExprs, PureExpr(Value(parameter)))
  )

  def prepParameters(
      expr: Expr
  ): ContextState[Tuple2[List[BindExpr], String]] =
    expr match {
      case b @ BindExpr(e, v, n) =>
        ((List(b), v)).pure[ContextState]
      case MultiLineExpr(exprs, r) =>
        prepParameters(r).map { case (prepExprs, result) =>
          (exprs :++ prepExprs, result)
        }
      case ComplexExpr(e, i) =>
        addTempVariable().flatMap(p =>
          prepParameters(BindExpr(s"$p <- $e", p, i))
        )
      case Value(v) if v.contains("'") =>
        addTempVariable().flatMap(p =>
          prepParameters(BindExpr(s"$p <- fetch $v", p))
        )
      case Value(v) => StateT.pure((List(), v))
    }

  def getResult(
      expr: Expr
  ): Tuple2[List[BindExpr], Expr] = expr match {
    case l: BindExpr             => (List(l), Value(l.v))
    case MultiLineExpr(exprs, r) => (exprs, r)
    case i: Value                => (List(), i)
    case e: ComplexExpr          => (List(), e)
    case c: CaseExpr             => (List(), DoExpr(c, c.indent))
  }

  def toParamVariable(v: String): String = v match {
    case WildcardName                                            => ""
    case s if s.startsWith(Desugar.RecursiveFunctionParamPrefix) => ""
    case _                                                       => v
  }

  def cTag(tag: String) = s"C$tag"

  def isNodeValue(t: Term): ContextState[Boolean] = typeCheck(t)
    .flatMap(TypeChecker.isRecursiveType(_))

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

  def getNameFromType(ty: Type): ContextState[String] =
    getNameFromIndex(TypeChecker.findRootTypeIndex(ty).get)

  def getNameFromIndex(idx: Int): ContextState[String] =
    State.inspect { ctx => Context.indexToName(ctx, idx).get }

  def methodToName(n: String): String =
    s"${n.filter(_.isLetterOrDigit)}'"

  def recordConstrToName(n: String): String =
    n.stripPrefix(Desugar.RecordConstrPrefix)

  def addTempVariable(): ContextState[String] =
    Context.addBinding("p", TempVarBind)

  def typeCheck(term: Term): ContextState[Type] =
    toContextState(TypeChecker.pureTypeOf(term))

  def toContextState[T](stateEither: StateEither[T]): ContextState[T] =
    stateEither.value.map(v =>
      v match {
        case Right(v) => v
        case Left(e) =>
          throw new RuntimeException(e)
      }
    )
}
