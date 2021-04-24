package core

import core.Context._
import parser.FuseParser._
import cats.implicits._
import scala.util._

object TypeChecker {

  def typeCheck(bindings: List[Binding], c: Context): List[String] =
    bindings
      .foldLeft((c, List[String]()))((acc, b) =>
        evalBinding(acc._1, b) match {
          case Right(c) => (c, acc._2)
          case Left(e)  => (c, e :: acc._2)
        }
      )
      ._2

  def evalBinding(c: Context, b: Binding): Either[String, Context] =
    ???

  def typeOf(ctx: Context, term: Term): Either[String, Type] = term match {
    case TermAscribe(t1, tyT2) =>
      for {
        _ <- checkKindStar(ctx, tyT2)
        tyT1 <- typeOf(ctx, t1)
        t <- isTypeEqual(ctx, tyT1, tyT2) match {
          case true  => Right(tyT2)
          case false => Left(Error.AscribeWrongType)
        }
      } yield t
    case TermVar(i, _) => Context.getTypeFromContext(ctx, i)
    case TermClosure(v, Some(tyT1), e) =>
      val ctx1 = Context.addBinding(ctx, v, VarBind(tyT1))
      typeOf(ctx1, e).map(tyT2 => TypeArrow(tyT1, typeShift(-1, tyT2)))
    case TermClosure(_, None, _) =>
      throw new Exception("Closure without annotation not supported.")
    case TermAbs(v, tyT1, e, _) =>
      val ctx1 = Context.addBinding(ctx, v, VarBind(tyT1))
      for {
        _ <- checkKindStar(ctx, tyT1)
        tyT2 <- typeOf(ctx1, e)
      } yield TypeArrow(tyT1, typeShift(-1, tyT2))
    case TermApp(t1, t2) =>
      for {
        tyT1 <- typeOf(ctx, t1)
        tyT2 <- typeOf(ctx, t2)
        ty <- simplifyType(ctx, tyT1) match {
          case TypeArrow(tyT11, tyT12) =>
            if (isTypeEqual(ctx, tyT2, tyT11)) Right(tyT12)
            else Left(Error.AppParameterTypeMismatch)
          case _ => Left(Error.AppNotArrowType)
        }
      } yield ty
    case TermProj(t, l) =>
      typeOf(ctx, t).flatMap(simplifyType(ctx, _) match {
        case TypeRecord(fields) =>
          fields
            .find { case (f, _) => f == l }
            .map(_._2)
            .toRight(Error.ProjFieldNotFound(l))
        case _ => Left(Error.ProjNotRecordType)
      })
    case TermFix(t1) =>
      typeOf(ctx, t1).flatMap(simplifyType(ctx, _) match {
        case TypeArrow(tyT11, tyT12) =>
          if (isTypeEqual(ctx, tyT12, tyT11)) Right(tyT12)
          else Left(Error.FixWrongBodyType)
        case _ => Left(Error.FixNotArrowType)
      })
    case TermRecord(fields) =>
      val fieldTypes = fields.traverse { case (v, term) =>
        typeOf(ctx, term).map((v, _))
      }
      fieldTypes.map(TypeRecord(_))
    case TermFold(tyS) =>
      simplifyType(ctx, tyS) match {
        case TypeRec(_, _, tyT) =>
          Right(TypeArrow(typeSubstituteTop(tyS, tyT), tyS))
        case _ => Left(Error.FoldNotRecursiveType)
      }
    case TermUnfold(tyS) =>
      simplifyType(ctx, tyS) match {
        case TypeRec(_, _, tyT) =>
          Right(TypeArrow(tyS, typeSubstituteTop(tyS, tyT)))
        case _ => Left(Error.UnfoldNotRecursiveType)
      }
    case TermLet(x, t1, t2) =>
      for {
        tyT1 <- typeOf(ctx, t1)
        ctx1 = Context.addBinding(ctx, x, VarBind(tyT1))
        tyT2 <- typeOf(ctx1, t2)
      } yield typeShift(-1, tyT2)
    case TermTAbs(v, t) =>
      val ctx1 = Context.addBinding(ctx, v, TypeVarBind(KindStar))
      typeOf(ctx, t).map(TypeAll(v, KindStar, _))
    case TermTApp(t1, ty2) =>
      for {
        k2 <- kindOf(ctx, ty2)
        ty1 <- typeOf(ctx, t1)
        tyT1 = simplifyType(ctx, ty1)
        ty <- tyT1 match {
          case TypeAll(_, k1, tyT2) =>
            if (k1 == k2) Right(typeSubstituteTop(ty2, tyT2))
            else Left(Error.TAppKindMismatch)
          case _ => Left(Error.TAppNotAllType)
        }
      } yield ty
    case TermMatch(t1, cases) =>
      for {
        ty1 <- typeOf(ctx, t1)
        tyT1 = simplifyType(ctx, ty1)
        tys <- cases.traverse((v) =>
          for {
            tyP <- patternOfType(ctx, v._1)
            _ <-
              if (tyP.isEmpty || isTypeEqual(ctx, tyT1, tyP.get)) Right()
              else Left(Error.MatchPatternTypeMismatch)
            ty2 <- typeOf(ctx, v._2)
          } yield ty2
        )
        ty <-
          if (tys.forall(isTypeEqual(ctx, _, tys.head))) Right(tys.head)
          else Left(Error.MatchCasesTypeMismach)
      } yield ty
    case TermTag(i, t1, ty1) =>
      simplifyType(ctx, ty1) match {
        case TypeVariant(fields) =>
          for {
            tyTiExpected <- fields
              .find(_._1 == i)
              .map(_._2)
              .toRight(Error.TagVariantFieldNotFound(i))
            tyTi <- typeOf(ctx, t1)
            ty <-
              if (isTypeEqual(ctx, tyTi, tyTiExpected)) Right(ty1)
              else Left(Error.TagFieldTypeMismatch)
          } yield ty
        case _ => Left(Error.TagNotVariantType)
      }
    case TermTrue      => Right(TypeBool)
    case TermFalse     => Right(TypeBool)
    case TermInt(_)    => Right(TypeInt)
    case TermFloat(_)  => Right(TypeFloat)
    case TermString(_) => Right(TypeString)
    case TermUnit      => Right(TypeUnit)

  }

  def kindOf(ctx: Context, t: Type): Either[String, Kind] = t match {
    case TypeArrow(ty1, ty2) =>
      for {
        _ <- checkKindStar(ctx, ty1)
        _ <- checkKindStar(ctx, ty1)
      } yield KindStar
    case TypeVar(i, _) => getKind(ctx, i)
    case TypeRecord(fields) =>
      fields
        .traverse(f => checkKindStar(ctx, f._2))
        .map(_ => KindStar)
    case TypeAll(tyX, k1, tyT2) =>
      val ctx1 = Context.addBinding(ctx, tyX, TypeVarBind(k1))
      checkKindStar(ctx1, tyT2)
    case TypeAbs(_, tyT2) => kindOf(ctx, tyT2).map(KindArrow(KindStar, _))
    case TypeApp(tyT1, tyT2) =>
      for {
        k1 <- kindOf(ctx, tyT1)
        k2 <- kindOf(ctx, tyT2)
        k <- k1 match {
          case KindArrow(k11, k12) =>
            if (k11 == k2) Right(k12) else Left(Error.KindAppParameterMismatch)
          case _ => Left(Error.NotArrowKind)
        }
      } yield k

    case _ => Right(KindStar)
  }

  def checkKindStar(ctx: Context, ty: Type): Either[String, Kind] =
    kindOf(ctx, ty).flatMap(_ == KindStar match {
      case true  => Right(KindStar)
      case false => Left(Error.KindStarExpected)
    })

  def getKind(ctx: Context, idx: Int): Either[String, Kind] =
    Context
      .getBinding(ctx, idx)
      .flatMap(_ match {
        case TypeVarBind(k)          => Right(k)
        case TypeAbbBind(_, Some(k)) => Right(k)
        case TypeAbbBind(_, None)    => Left(Error.NoKindForVariable(ctx, idx))
        case _                       => Left(Error.WrongKindBindingForVariable(ctx, idx))
      })

  def patternOfType(ctx: Context, p: Pattern): Either[String, Option[Type]] =
    ???

  def typeShift(v: Int, t: Type): Type = ???

  def isTypeEqual(ctx: Context, ty1: Type, ty2: Type): Boolean = ???

  def simplifyType(ctx: Context, ty: Type): Type = {
    val tyT = ty match {
      case TypeApp(tyT1, tyT2) => TypeApp(simplifyType(ctx, tyT1), tyT2)
      case _                   => ty
    }
    computeType(ctx, ty) match {
      case Some(ty1) => simplifyType(ctx, ty1)
      case _         => ty
    }
  }

  def computeType(ctx: Context, ty: Type): Option[Type] = ty match {
    case TypeVar(idx, _) => getTypeAbb(ctx, idx)
    case TypeApp(TypeAbs(_, tyT12), tyT2) =>
      Some(typeSubstituteTop(tyT2, tyT12))
    case _ => None
  }

  def getTypeAbb(ctx: Context, index: Int): Option[Type] =
    Context
      .getBinding(ctx, index)
      .toOption
      .flatMap(_ match {
        case TypeAbbBind(ty, _) => Some(ty)
        case _                  => None
      })

  def typeSubstituteTop(ty1: Type, ty2: Type): Type = ???

  object Error {
    // Type Errors
    val AscribeWrongType = "Body of as-term does not have the expected type."
    val AppParameterTypeMismatch = "Parameter type mismatch."
    val AppNotArrowType = "Arrow type expected on application."
    def ProjFieldNotFound(f: String) = s"Field $f not found on the record."
    val ProjNotRecordType = "Expected record type."
    val FixWrongBodyType = "Type of body not compatible with the domain."
    val FixNotArrowType = "Arrow type expected for fix combinator."
    val FoldNotRecursiveType = "Recursive type expected on fold."
    val UnfoldNotRecursiveType = "Recursive type expected on unfold."
    val TAppKindMismatch = "Type argument has wrong kind."
    val TAppNotAllType = "Universal type expected on type application."
    val MatchPatternTypeMismatch =
      "Pattern type is not the same as the type of the match expression."
    val MatchCasesTypeMismach = "Type between cases don't match."
    val TagNotVariantType = "Expected variant type on the tag."
    def TagVariantFieldNotFound(f: String) =
      s"Fied $f not found on the variant."
    val TagFieldTypeMismatch = "Field does not have an expected type."

    // Kind Errors
    val KindStarExpected = "Kind * expected."
    def NoKindForVariable(ctx: Context, idx: Int) =
      s"No kind recorded for type ${Context.indexToName(ctx, idx).get}"
    def WrongKindBindingForVariable(ctx: Context, idx: Int) =
      s"getKind: Wrong kind of binding for variable ${Context.indexToName(ctx, idx).get}"
    val KindAppParameterMismatch = "Parameter kind mismatch."
    val NotArrowKind = "Arrow kind expected"
  }

}
