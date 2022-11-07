package code

import cats.data.State
import cats.data.StateT
import cats.implicits.*
import core.Bindings.*
import core.Context
import core.Context.*
import core.Desugar
import core.Terms.*
import core.TypeChecker
import core.Types.*
import parser.Info.UnknownInfo

object GrinUtils {
  val dummyType = TypeUnit(UnknownInfo)

  def cTag(tag: String) = s"C$tag"

  def pTag(arity: Int, tag: String) = s"P$arity$tag"

  def isNodeValue(t: Term): ContextState[Boolean] = typeCheck(t)
    .flatMap(TypeChecker.isRecursiveType(_))

  def isFunctionType(ty: Type): ContextState[Boolean] = for {
    tyS <- TypeChecker.simplifyType(ty)
    value = getFunctionArity(tyS) match {
      case v if v > 0 => true
      case _          => false
    }
  } yield value

  def getFunctionArity(ty: Type): Int = ty match {
    case TypeAbs(_, _, a: TypeArrow)   => getFunctionArity(a)
    case TypeArrow(_, _, a: TypeArrow) => 1 + getFunctionArity(a)
    case TypeArrow(_, _, _)            => 1
    case _                             => 0
  }

  def freeVars(term: Term): ContextState[List[(String, String)]] =
    Context.run(extractFreeVars(term))

  def extractFreeVars(term: Term): ContextState[List[(String, String)]] =
    term match {
      case TermVar(info, idx, ctxLength) =>
        toContextState(Context.getBinding(info, idx))
          .flatMap(_ match {
            case VarBind(_) =>
              getNameFromIndex(idx).flatMap(v =>
                addTempVariable(v).map { p => List((v, p)) }
              )
            case _ =>
              State.pure(Nil)
          })
      case TermClosure(_, variable, _, e) =>
        for {
          _ <- Context.addName(variable)
          v <- freeVars(e)
        } yield v
      case TermMatch(_, t, c) =>
        for {
          v1 <- freeVars(t)
          v2 <- c.traverse { case (p, e) =>
            p match {
              case PatternNode(_, node, vars) =>
                for {
                  _ <- vars.traverse(Context.addName(_))
                  v <- freeVars(e)
                } yield v
              case _ => freeVars(e)
            }
          }
        } yield v1 :++ v2.flatten
      case TermLet(_, variable, t1, t2) =>
        for {
          v1 <- freeVars(t1)
          _ <- Context.addName(variable)
          v2 <- freeVars(t2)
        } yield v1 :++ v2
      case TermApp(_, t1, t2) =>
        for {
          v1 <- freeVars(t1)
          v2 <- freeVars(t2)
        } yield v1 :++ v2
      case TermProj(_, t, _)       => freeVars(t)
      case TermMethodProj(_, t, _) => freeVars(t)
      case TermAscribe(_, t, _)    => freeVars(t)
      case TermTAbs(_, _, t)       => freeVars(t)
      case TermTApp(_, t, _)       => freeVars(t)
      case _                       => State.pure(Nil)

    }

  def addTempVariable(name: String = "p"): ContextState[String] =
    pickFreshName(name)

  def getNameFromType(ty: Type): ContextState[String] =
    getNameFromIndex(TypeChecker.findRootTypeIndex(ty).get)

  def getNameFromIndex(idx: Int): ContextState[String] =
    State.inspect { ctx => Context.indexToName(ctx, idx).get }

  def typeCheck(term: Term): ContextState[Type] =
    toContextState(TypeChecker.pureInfer(term))

  def toContextState[T](stateEither: StateEither[T]): ContextState[T] =
    stateEither.value.map(v =>
      v match {
        case Right(v) => v
        case Left(e) =>
          throw new RuntimeException(e)
      }
    )
}
