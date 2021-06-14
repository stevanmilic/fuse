package core

import cats.data.EitherT
import cats.data.OptionT
import cats.data.State
import cats.data.StateT
import cats.implicits._

import scala.util._

import Shifting._

object Context {
  type Context = List[(String, Binding)]

  type Error = String
  type ContextState[A] = State[Context, A]
  type StateEither[A] = EitherT[ContextState, Error, A]
  // TODO: Use this instead of plain Option[A] in Desugar object.
  type StateOption[A] = OptionT[ContextState, A]

  val empty = List[(String, Binding)]()
  val WildcardName = "_"

  def addName(n: String): ContextState[String] =
    State { ctx => ((n, NameBind) :: ctx, n) }

  def addBinding(n: String, b: Binding): ContextState[String] =
    State { ctx => ((n, b) :: ctx, n) }

  def isNameBound(c: Context, x: String): Boolean =
    c.exists { case ((i, _)) => i == x }

  def pickFreshName(c: Context, x: String): ContextState[String] =
    isNameBound(c, x) match {
      case true  => pickFreshName(c, s"$x'")
      case false => addName(x)
    }

  def indexToName(c: Context, x: Int): Option[String] =
    c.lift(x).map { case ((i, _)) => i }

  def nameToIndex(c: Context, x: String): Option[Int] =
    c.indexWhere { case ((i, _)) => i == x } match {
      case v if v >= 0 => Some(v)
      case _           => None
    }

  def run[T](state: ContextState[T]): ContextState[T] =
    State { ctx => (ctx, state.runA(ctx).value) }

  def runE[A](state: StateEither[A]): StateEither[A] =
    EitherT { State(ctx => (ctx, state.value.runA(ctx).value)) }

  def getType(idx: Int): StateEither[Type] =
    getBinding(idx).flatMap[Error, Type](_ match {
      case VarBind(ty)              => EitherT.rightT(ty)
      case TermAbbBind(_, Some(ty)) => EitherT.rightT(ty)
      case TermAbbBind(_, None) =>
        EitherT.left(
          State.inspect(ctx =>
            s"No type recorded for variable ${indexToName(ctx, idx)}"
          )
        )
      case _ =>
        EitherT.left(
          State.inspect(ctx =>
            s"getTypeFromContext: Wrong kind of binding for variable ${indexToName(ctx, idx)}"
          )
        )
    })

  def getMethodType(typeIdx: Int, methodName: String): StateEither[Type] = for {
    typeName <- EitherT(
      State.inspect { (ctx: Context) =>
        indexToName(ctx, typeIdx).toRight("Type not found")
      }
    )
    methodIdx <- EitherT(
      State.inspect { (ctx: Context) =>
        nameToIndex(ctx, Desugar.toMethodId(methodName, typeName))
          .toRight(s"Method $methodName not found")
      }
    )
    methodType <- getType(methodIdx)
  } yield methodType

  def getBinding(idx: Int): StateEither[Binding] = EitherT(
    State.inspect { ctx =>
      ctx.lift(idx) match {
        case Some((_, b)) => Right(bindingShift(idx + 1, b))
        case _ =>
          Left(
            s"Variable not found: offset $idx, ctx size ${ctx.length}"
          )
      }
    }
  )

}
