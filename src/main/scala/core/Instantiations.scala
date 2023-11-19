package core

import cats.data.EitherT
import cats.data.OptionT
import cats.data.State
import cats.data.StateT
import cats.implicits.*
import core.Context.*
import core.Bindings.*
import core.Types.Type
import core.Terms.*
import core.TypeChecker.*
import core.Types.*
import parser.Info.Info

object Instantiations {
  case class Instantiation(info: Info, i: String, idx: Int, tys: List[Type])

  def build(
      t: Term,
      solutions: List[TypeESolutionBind]
  ): StateEither[List[Instantiation]] = (t, solutions) match {
    case (_, Nil) => Nil.pure
    case (TermVar(info, idx, _), _) =>
      for {
        optionName <- EitherT.liftF(State.inspect { (ctx: Context) =>
          indexToName(ctx, idx)
        })
        name <- optionName match {
          case Some(name) => name.pure[StateEither]
          case None       => TypeError.format(NotFoundTypeError(info))
        }
        tys = solutions.map(_.t)
      } yield List(Instantiation(info, name, idx, tys))
    case _ => Nil.pure
  }
}
