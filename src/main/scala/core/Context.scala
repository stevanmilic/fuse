package core

import cats.data.EitherT
import cats.data.OptionT
import cats.data.State
import cats.data.StateT
import cats.implicits.*
import core.Bindings.*
import core.Terms.*
import core.Types.*
import parser.Info.*

import scala.util.*

import Shifting.*

object Context {
  type Context = Tuple2[List[(String, Binding)], Int]

  type Error = String
  type ContextState[A] = State[Context, A]
  type StateEither[A] = EitherT[ContextState, Error, A]
  type StateOption[A] = OptionT[ContextState, A]

  val emptyContext = (List[(String, Binding)](), 0)
  val WildcardName = "_"

  def addName(n: String): ContextState[String] = addBinding(n, NameBind)

  def addBinding(n: String, b: Binding): ContextState[String] =
    State(ctx => {
      val currVarBindings = ctx._2
      val (name, numOfVarBindings) = b match {
        case _: VarBind if n.startsWith(Desugar.RecursiveFunctionParamPrefix) =>
          (n, currVarBindings)
        case _: VarBind =>
          (s"$n$currVarBindings", currVarBindings + 1)
        case _ => (n, currVarBindings)
      }
      (((name, b) :: ctx._1, numOfVarBindings), name)
    })

  def isNameBound(x: String): ContextState[Boolean] =
    State.inspect(c => c._1.exists { case (i, _) => i == x })

  def pickFreshName(x: String): ContextState[String] =
    State(ctx => {
      val c = ctx.map(_ + 1)
      (c, s"$x${c._2}")
    })

  def indexToName(c: Context, x: Int): Option[String] =
    c._1.lift(x).map { case ((i, _)) => i }

  def nameToIndex(c: Context, x: String): Option[Int] =
    c._1.indexWhere { case ((i, _)) => i == x } match {
      case v if v >= 0 => Some(v)
      case _           => None
    }

  def run[T](state: ContextState[T]): ContextState[T] =
    State { ctx =>
      {
        val (ctx1, t) = state.run(ctx).value
        ((ctx._1, ctx._2 + (ctx1._2 - ctx._2)), t)
      }
    }

  def runE[A](state: StateEither[A]): StateEither[A] =
    EitherT { run(state.value) }

  def getType(info: Info, idx: Int): StateEither[Type] =
    getBinding(info, idx).flatMap[Error, Type](_ match {
      case VarBind(ty)              => EitherT.rightT(ty)
      case TermAbbBind(_, Some(ty)) => EitherT.rightT(ty)
      case TermAbbBind(_, None) =>
        TypeError.format(NoTypeForVariableTypeError(info, idx))
      case TypeAbbBind(ty, _) => EitherT.rightT(ty)
      case _ =>
        TypeError.format(WrongBindingForVariableTypeError(info, idx))
    })

  def getBinding(info: Info, idx: Int): StateEither[Binding] =
    EitherT
      .liftF(State.inspect { (ctx: Context) =>
        ctx._1.lift(idx)
      })
      .flatMap(_ match {
        case Some((_, b)) => bindingShift(idx + 1, b).pure[StateEither]
        case _            => TypeError.format(BindingNotFoundTypeError(info))
      })
}
