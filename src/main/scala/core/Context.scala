package core

import cats.data.EitherT
import cats.data.OptionT
import cats.data.State
import cats.data.StateT
import cats.implicits._
import core.Bindings._
import core.Terms._
import core.Types._
import parser.Info._

import scala.util._

import Shifting._

object Context {
  type Context = List[(String, Binding)]

  type Error = String
  type ContextState[A] = State[Context, A]
  type StateEither[A] = EitherT[ContextState, Error, A]
  type StateOption[A] = OptionT[ContextState, A]

  val emptyContext = List[(String, Binding)]()
  val WildcardName = "_"

  def addName(n: String): ContextState[String] =
    State { ctx => ((n, NameBind) :: ctx, n) }

  def addBinding(n: String, b: Binding): ContextState[String] =
    State { ctx => ((n, b) :: ctx, n) }

  def isNameBound(x: String): ContextState[Boolean] =
    State.inspect(c => c.exists { case (i, _) => i == x })

  def pickFreshName(x: String): ContextState[String] =
    isNameBound(x).flatMap(_ match {
      case true  => pickFreshName(s"$x'")
      case false => addName(x)
    })

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

  def getType(info: Info, idx: Int): StateEither[Type] =
    getBinding(info, idx).flatMap[Error, Type](_ match {
      case VarBind(ty)              => EitherT.rightT(ty)
      case TermAbbBind(_, Some(ty)) => EitherT.rightT(ty)
      case TermAbbBind(_, None) =>
        TypeError.format(NoTypeForVariableTypeError(info, idx))
      case _ =>
        TypeError.format(WrongBindingForVariableTypeError(info, idx))
    })

  def getBinding(info: Info, idx: Int): StateEither[Binding] =
    EitherT
      .liftF(State.inspect { (ctx: Context) => ctx.lift(idx) })
      .flatMap(_ match {
        case Some((_, b)) => bindingShift(idx + 1, b).pure[StateEither]
        case _            => TypeError.format(BindingNotFoundTypeError(info))
      })
}
