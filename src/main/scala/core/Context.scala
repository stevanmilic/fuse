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
import scala.annotation.tailrec

import scala.util.*

import Shifting.*

object Context {
  type Note = (String, Binding)
  type Context = Tuple2[List[Note], Int]

  type Error = String
  type ContextState[A] = State[Context, A]
  type StateEither[A] = EitherT[ContextState, Error, A]
  type StateOption[A] = OptionT[ContextState, A]

  val emptyContext = (List[(String, Binding)](), 0)
  val WildcardName = "_"

  def addName(n: String): ContextState[String] = addBinding(n, NameBind)

  def getNotes(ctx: Context, withMarks: Boolean = false): LazyList[Note] = {
    val (l, c) = ctx
    withMarks match {
      case false =>
        l.to(LazyList).filter {
          case (_, _: Mark) => false
          case (_, _)       => true
        }
      case true => l.to(LazyList)
    }
  }

  def addBinding(n: String, b: Binding): ContextState[String] = {
    def incrVarBindings(name: String, numOfVarBindings: Int) = b match {
      case _: VarBind if n.startsWith(Desugar.RecursiveFunctionParamPrefix) =>
        (n, numOfVarBindings)
      case _: VarBind | _: TypeVarBind =>
        (s"$n$numOfVarBindings", numOfVarBindings + 1)
      case _ => (n, numOfVarBindings)
    }
    State(ctx => {
      val (notes, currVarBindings) = ctx
      val (name, numOfVarBindings) = incrVarBindings(n, currVarBindings)
      val ctx1 = ((name, b) :: notes, numOfVarBindings)
      (ctx1, name)
    })
  }

  def pickFreshName(x: String): ContextState[String] =
    State(ctx => {
      val c = ctx.map(_ + 1)
      (c, s"$x${c._2}")
    })

  def indexToName(
      c: Context,
      x: Int,
      withMarks: Boolean = false
  ): Option[String] =
    getNotes(c, withMarks).lift(x).map { case ((i, _)) => i }

  def nameToIndex(
      c: Context,
      x: String,
      withMarks: Boolean = false
  ): Option[Int] =
    getNotes(c, withMarks).indexWhere { case ((i, _)) => i == x } match {
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

  def getBinding(
      info: Info,
      idx: Int,
      withMarks: Boolean = false
  ): StateEither[Binding] =
    EitherT
      .liftF(State.inspect { (ctx: Context) =>
        getNotes(ctx, withMarks).lift(idx)
      })
      .flatMap(_ match {
        case Some((_, b)) => bindingShift(idx + 1, b).pure[StateEither]
        case _            => TypeError.format(BindingNotFoundTypeError(info))
      })

  def getAlgebraicDataTypeVar(
      info: Info,
      node: String
  ): ContextState[Option[TypeVar]] =
    State.inspect { (ctx: Context) =>
      val notes = getNotes(ctx).toList
      notes.zipWithIndex.collectFirst {
        case ((i, TypeAbbBind(_, _)), idx) if i == node =>
          TypeVar(info, idx, notes.length)
        case ((_, TypeAbbBind(ty, _)), idx) if isSumType(ty, node) =>
          TypeVar(info, idx, notes.length)
      }
    }

  /** Shifts type based on the difference between TypeVar length and current
    * length of the context.
    *
    * Required to do so when an existential variables gets resolved _deep_ in
    * the context but doesn't get shifted once _out_ of it. Usually that happens
    * in closures.
    */
  def typeShiftOnContextDiff(ty: Type): ContextState[Type] = ty match {
    case TypeVar(_, _, varContextLength) =>
      State.inspect { ctx =>
        val d = varContextLength - getNotes(ctx).toList.length
        typeShift(-d, ty)
      }
    case TypeApp(info, ty1, ty2) =>
      for {
        ty1S <- typeShiftOnContextDiff(ty1)
        ty2S <- typeShiftOnContextDiff(ty2)
      } yield TypeApp(info, ty1S, ty2S)
    case _ => ty.pure
  }

  @tailrec
  def isSumType(t: Type, node: String): Boolean = t match {
    case TypeAbs(_, _, ty) => isSumType(ty, node)
    case TypeRec(_, _, _, TypeVariant(_, fields))
        if fields.exists(_._1 == node) =>
      true
    case _ => false
  }

  // region: existential variables

  def addEVar(x: String, info: Info, mark: Mark): ContextState[TypeEVar] = for {
    freshName <- pickFreshName(x)
    eA <- addBinding(freshName, mark)
  } yield TypeEVar(info, freshName)

  def addMark(x: String): ContextState[String] = for {
    freshName <- pickFreshName(x)
    m <- addBinding(freshName, TypeEMarkBind)
  } yield m

  def freshEVar(x: String, info: Info): ContextState[TypeEVar] =
    pickFreshName(x).map(TypeEVar(info, _))

  def containsFreeEVar(eV: TypeEVar): ContextState[Boolean] =
    State.inspect { ctx =>
      getNotes(ctx, withMarks = true).find {
        case (v, TypeEFreeBind) if v == eV.name => true
        case _                                  => false
      }.isDefined
    }

  def solve(eA: TypeEVar, ty: Type): StateEither[Unit] = for {
    idx <- getFreeEVarIndex(eA)
    s <- EitherT.liftF(State.modify { (ctx: Context) =>
      (
        getNotes(ctx, withMarks = true).toList
          .updated(idx, (eA.name, TypeESolutionBind(ty))),
        ctx._2
      )
    })
  } yield s

  def insertEArrow(
      eA: TypeEVar
  ): StateEither[Tuple3[TypeEVar, TypeEVar, TypeArrow]] = for {
    ty1 <- EitherT.liftF(freshEVar("a1", eA.info))
    ty2 <- EitherT.liftF(freshEVar("a2", eA.info))
    ty = TypeArrow(eA.info, ty1, ty2)
    notes = List(
      (eA.name, TypeESolutionBind(ty)),
      (ty1.name, TypeEFreeBind),
      (ty2.name, TypeEFreeBind)
    )
    idx <- getFreeEVarIndex(eA)
    s <- EitherT.liftF(State.modify { (ctx: Context) =>
      {
        val (postNotes, preNotes) =
          getNotes(ctx, withMarks = true).toList.splitAt(idx)
        (postNotes ::: notes ::: preNotes.tail, ctx._2)
      }
    })
  } yield (ty1, ty2, ty)

  def insertEApp(
      eA: TypeEVar
  ): StateEither[Tuple3[TypeEVar, TypeEVar, TypeApp]] = for {
    ty1 <- EitherT.liftF(freshEVar("a1", eA.info))
    ty2 <- EitherT.liftF(freshEVar("a2", eA.info))
    ty = TypeApp(eA.info, ty1, ty2)
    notes = List(
      (eA.name, TypeESolutionBind(ty)),
      (ty1.name, TypeEFreeBind),
      (ty2.name, TypeEFreeBind)
    )
    idx <- getFreeEVarIndex(eA)
    s <- EitherT.liftF(State.modify { (ctx: Context) =>
      {
        val (postNotes, preNotes) =
          getNotes(ctx, withMarks = true).toList.splitAt(idx)
        (postNotes ::: notes ::: preNotes.tail, ctx._2)
      }
    })
  } yield (ty1, ty2, ty)

  def getFreeEVarIndex(eA: TypeEVar): StateEither[Int] = for {
    idx <- EitherT.liftF(State.inspect { (ctx: Context) =>
      getNotes(ctx, withMarks = true).indexWhere {
        case (v, TypeEFreeBind) if v == eA.name => true
        case _                                  => false
      } match {
        case v if v >= 0 => Some(v)
        case _           => None
      }
    })
    r <- idx match {
      case Some(v) => v.pure[StateEither]
      case None    => TypeError.format(ExistentialVariableNotFoundTypeError(eA))
    }
  } yield r

  /** Looks up the solution for `ev` in `ctx`. */
  def solution(
      eV: TypeEVar,
      shift: Boolean = true
  ): ContextState[Option[Type]] =
    State.inspect { ctx =>
      val notes = getNotes(ctx, withMarks = true)
      val t = notes.collectFirst {
        case (v, TypeESolutionBind(ty)) if v == eV.name => ty
      }
      // NOTE: We are shifting the existential variable solution by the number
      // of "regular" (non-mark) bindings untill its place in the context. This
      // is required because we are not using indexes for the evars, therefore
      // if any regular binding is inserted afterwards the index in the
      // solution type is incorrect – cause we haven't shifted it. So we
      // "manually" gotta do that, count the bindings and shift.
      t.map(ty =>
        if (shift) typeShift(countBindingsTillEVar(notes, eV), ty) else ty
      )
    }

  def countBindingsTillEVar(notes: LazyList[Note], eV: TypeEVar): Int =
    notes
      .takeWhile { case (v, _) => v != eV.name }
      .filter {
        case (_, _: Mark) => false
        case _            => true
      }
      .size

  def isWellFormedWithPeel(ty: Type, eA: TypeEVar): ContextState[Boolean] =
    Context.run {
      for {
        _ <- peel(eA.name)
        r <- isWellFormed(ty)
      } yield r
    }

  def isWellFormed(ty: Type): ContextState[Boolean] = ty match {
    case eA: TypeEVar =>
      for {
        hasEVar <- containsFreeEVar(eA)
        r <- hasEVar match {
          case true  => solution(eA).map(_.isDefined)
          case false => false.pure[ContextState]
        }
      } yield r
    case TypeArrow(_, ty1, ty2) =>
      for {
        r1 <- isWellFormed(ty1)
        r2 <- isWellFormed(ty2)
      } yield r1 && r2
    case TypeApp(_, ty1, ty2) =>
      for {
        r1 <- isWellFormed(ty1)
        r2 <- isWellFormed(ty2)
      } yield r1 && r2
    case TypeAll(_, _, _, ty) => isWellFormed(ty)
    case TypeRecord(_, l) =>
      l.traverse { case (_, ty) => isWellFormed(ty) }.map(_.reduce(_ && _))
    case TypeVariant(_, l) =>
      l.traverse { case (_, ty) => isWellFormed(ty) }.map(_.reduce(_ && _))
    case TypeRec(_, _, _, ty) => isWellFormed(ty)
    case _                    => true.pure[ContextState]
  }

  def containsEVarWithPeel(
      eA: TypeEVar,
      peelToEVar: Type
  ): ContextState[Boolean] = peelToEVar match {
    case TypeEVar(_, name) =>
      Context.run {
        for {
          _ <- peel(name, true)
          r <- containsFreeEVar(eA)
        } yield r
      }
    case _ => false.pure
  }

  /** Peels off the end of a context up to and including `note`. */
  def peel(name: String, emptyOnNotFound: Boolean = false): ContextState[Unit] =
    State.modify { (ctx: Context) =>
      nameToIndex(ctx, name, withMarks = true) match {
        case Some(_) =>
          val notes = getNotes(ctx, withMarks = true).dropWhile {
            case (v, _) if v == name => false
            case _                   => true
          }.toList
          (
            notes match {
              case h :: t => t
              case Nil    => Nil
            },
            ctx._2
          )
        case None => if (emptyOnNotFound) emptyContext else ctx
      }
    }

  // region end

  def print(withMarks: Boolean = true): ContextState[Unit] = {
    println()
    State.inspect { ctx =>
      getNotes(ctx, withMarks).zipWithIndex.foreach(e =>
        val repr =
          Representation.bindingToType(e._1._2).value.runA(ctx).value.merge
        println(s"${e._2}|${e._1._1}: $repr")
      )
    }
  }
}
