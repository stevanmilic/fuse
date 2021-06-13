package core

import cats.data.EitherT
import cats.data.State
import cats.implicits._

import Context._

object Representation {
  def typeToString(t: Type): StateEither[String] = t match {
    case TypeVar(idx, n) =>
      EitherT(State.inspect { ctx =>
        Context.indexToName(ctx, idx).toRight("Type variable not found.")
      })
    case TypeId(id) => id.pure[StateEither]
    case TypeAbs(typeVar, ty) =>
      for {
        _ <- EitherT.liftF(Context.addName(typeVar))
        ty1 <- Context.runE(typeToString(ty))
      } yield s"λ $typeVar. $ty1"
    case TypeArrow(t1, t2) =>
      for {
        s1 <- typeToString(t1)
        s2 <- typeToString(t2)
      } yield s"$s1 -> $s2"
    case TypeAll(typeVar, kind, ty) =>
      for {
        _ <- EitherT.liftF(Context.addName(typeVar))
        k1 <- kindToString(kind).pure[StateEither]
        ty1 <- Context.runE(typeToString(ty))
      } yield s"[$typeVar::$k1] $ty1"
    case TypeApp(ty1, ty2) =>
      for {
        ty1s <- typeToString(ty1)
        ty2s <- typeToString(ty2)
      } yield s"$ty1s[$ty2s]"
    case TypeRecord(fields) =>
      fields.traverse { case (fieldName, ty) =>
        typeToString(ty).map(typeString => s"$fieldName: $typeString")
      }.map(fieldTypes => s"{${fieldTypes.mkString(", ")}}")
    case TypeVariant(fields) =>
      fields.traverse { case (fieldName, ty) =>
        typeToString(ty).map(typeString => s"$fieldName$typeString")
      }.map(fieldTypes => s"${fieldTypes.mkString(" | ")}")
    case TypeRec(tyX, _, ty) => for {
      _ <- EitherT.liftF(Context.addName(tyX.stripPrefix("@")))
      ty1 <- typeToString(ty)
    } yield ty1
    case TypeUnit          => "()".pure[StateEither]
    case TypeInt           => "i32".pure[StateEither]
    case TypeFloat         => "f32".pure[StateEither]
    case TypeString        => "str".pure[StateEither]
    case TypeBool          => "bool".pure[StateEither]
  }

  def kindToString(k: Kind): String = k match {
    case KindStar => "*"
    case KindArrow(k1, k2) =>
      s"${kindToString(k1)} => ${kindToString(k2)}"
  }
}
