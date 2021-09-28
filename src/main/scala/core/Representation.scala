package core

import cats.data.EitherT
import cats.data.State
import cats.implicits._
import core.Types._
import core.Bindings._
import Context._

object Representation {
  def typeRepresentation(
      types: List[Bind]
  ): Either[String, List[String]] =
    types
      .traverse(bind => {
        val repr = bind.b match {
          case TypeVarBind(k) =>
            Representation.kindToString(k).pure[StateEither]
          case VarBind(ty) =>
            Context.runE(Representation.typeToString(ty, buildContext = true))
          case TypeAbbBind(_, Some(k)) =>
            Representation.kindToString(k).pure[StateEither]
          case TermAbbBind(_, Some(ty)) =>
            Context.runE(Representation.typeToString(ty, buildContext = true))
        }
        repr.map2(EitherT.liftF(addName(bind.i))) { case (repr, id) =>
          s"$id: $repr"
        }
      })
      .value
      .runA(emptyContext)
      .value

  def typeToString(
      t: Type,
      buildContext: Boolean = false
  ): StateEither[String] =
    t match {
      case TypeVar(_, idx, n) =>
        EitherT(State.inspect { ctx =>
          Context
            .indexToName(ctx, idx)
            .toRight("Repr: Type variable not found.")
        })
      case TypeId(_, id) => id.pure[StateEither]
      case TypeAbs(_, typeVar, ty) =>
        for {
          _ <- addNameToContext(typeVar, buildContext)
          ty1 <- Context.runE(typeToString(ty, buildContext))
        } yield s"λ $typeVar. $ty1"
      case TypeArrow(_, t1, t2) =>
        for {
          s1 <- typeToString(t1, buildContext)
          s2 <- typeToString(t2, buildContext)
        } yield s"$s1 -> $s2"
      case TypeAll(_, typeVar, kind, ty) =>
        for {
          _ <- addNameToContext(typeVar, buildContext)
          k1 <- kindToString(kind).pure[StateEither]
          ty1 <- Context.runE(typeToString(ty, buildContext))
        } yield s"[$typeVar::$k1] $ty1"
      case TypeApp(_, ty1, ty2) =>
        for {
          ty1s <- typeToString(ty1, buildContext)
          ty2s <- typeToString(ty2, buildContext)
        } yield s"$ty1s[$ty2s]"
      case TypeRecord(_, fields) =>
        fields
          .traverse { case (fieldName, ty) =>
            typeToString(ty, buildContext).map(typeString =>
              s"$fieldName: $typeString"
            )
          }
          .map(fieldTypes => s"{${fieldTypes.mkString(", ")}}")
      case TypeVariant(_, fields) =>
        fields
          .traverse { case (fieldName, ty) =>
            typeToString(ty, buildContext).map(typeString =>
              s"$fieldName$typeString"
            )
          }
          .map(fieldTypes => s"${fieldTypes.mkString(" | ")}")
      case TypeRec(_, tyX, _, ty) =>
        for {
          _ <- addNameToContext(tyX.stripPrefix("@"), buildContext)
          ty1 <- typeToString(ty, buildContext)
        } yield ty1
      case TypeUnit(_)   => "()".pure[StateEither]
      case TypeInt(_)    => "i32".pure[StateEither]
      case TypeFloat(_)  => "f32".pure[StateEither]
      case TypeString(_) => "str".pure[StateEither]
      case TypeBool(_)   => "bool".pure[StateEither]
    }

  def addNameToContext(name: String, skip: Boolean): StateEither[String] =
    skip match {
      case true  => EitherT.liftF(Context.addName(name))
      case false => "".pure[StateEither]
    }

  def kindToString(k: Kind): String = k match {
    case KindStar => "*"
    case KindArrow(k1, k2) =>
      s"${kindToString(k1)} => ${kindToString(k2)}"
  }
}
