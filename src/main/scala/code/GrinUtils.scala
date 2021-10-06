package code

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

  def getNameFromType(ty: Type): ContextState[String] =
    getNameFromIndex(TypeChecker.findRootTypeIndex(ty).get)

  def getNameFromIndex(idx: Int): ContextState[String] =
    State.inspect { ctx => Context.indexToName(ctx, idx).get }

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
