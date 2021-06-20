package core

import cats.data.EitherT
import cats.data.OptionT
import cats.data.State
import cats.data.StateT
import cats.implicits._
import core.Context._
import core.Shifting._
import parser.FuseParser._

import scala.util._
import scala.annotation.tailrec

object TypeChecker {

  def check(binds: List[Bind]): StateEither[List[Bind]] =
    binds.traverse(bind =>
      for {
        binding <- checkBinding(bind.b)
        id <- EitherT.liftF(addBinding(bind.i, binding))
      } yield Bind(id, binding)
    )

  def checkBinding(b: Binding): StateEither[Binding] = b match {
    case NameBind           => EitherT.rightT(NameBind)
    case t @ TypeVarBind(_) => EitherT.rightT(t)
    case v @ VarBind(_)     => EitherT.rightT(v)
    case TypeAbbBind(ty, None) =>
      pureKindOf(ty).map(k => TypeAbbBind(ty, Some(k)))
    case TermAbbBind(term, None) =>
      pureTypeOf(term).map(ty => TermAbbBind(term, Some(ty)))

  }

  def pureTypeOf(t: Term): StateEither[Type] = Context.runE(typeOf(t))

  def typeOf(term: Term): StateEither[Type] = term match {
    case TermAscribe(t1, typeToAscribe) =>
      for {
        _ <- checkKindStar(typeToAscribe)
        tyT1 <- pureTypeOf(t1)
        t <- EitherT(
          isTypeEqual(tyT1, typeToAscribe).map(
            Either.cond(_, typeToAscribe, Error.AscribeWrongType)
          )
        )
      } yield t
    case TermVar(idx, _) => Context.getType(idx)
    case TermClosure(variable, Some(variableType), expr) =>
      for {
        _ <- checkKindStar(variableType)
        _ <- EitherT.liftF(Context.addBinding(variable, VarBind(variableType)))
        tyT2 <- pureTypeOf(expr)
      } yield TypeArrow(variableType, typeShift(-1, tyT2))
    case TermClosure(_, None, _) =>
      throw new Exception("Closure without annotation not supported.")
    case TermAbs(variable, variableType, expr, _) =>
      for {
        _ <- checkKindStar(variableType)
        _ <- EitherT.liftF(Context.addBinding(variable, VarBind(variableType)))
        tyT2 <- pureTypeOf(expr)
      } yield TypeArrow(variableType, typeShift(-1, tyT2))
    case TermApp(t1, t2) =>
      for {
        tyT1 <- pureTypeOf(t1)
        tyT2 <- pureTypeOf(t2)
        tyT1S <- EitherT.liftF(simplifyType(tyT1))
        ty <- tyT1S match {
          case TypeArrow(tyT11, tyT12) =>
            EitherT(
              isTypeEqual(tyT2, tyT11).map(
                Either.cond(
                  _,
                  tyT12,
                  Error.AppParameterTypeMismatch
                )
              )
            )
          case _ =>
            EitherT.leftT[ContextState, Type](
              Error.AppNotArrowType
            )
        }
      } yield ty
    case TermProj(ty, label) =>
      for {
        tyT1 <- pureTypeOf(ty)
        tyT1S <- EitherT.liftF(simplifyType(tyT1))
        fieldType <- tyT1S match {
          case TypeRec(_, _, TypeRecord(fields)) =>
            EitherT.fromEither[ContextState](
              fields
                .find { case (f, _) => f == label }
                .map(_._2)
                .toRight(Error.ProjFieldNotFound(label))
            )
          case _ => EitherT.leftT[ContextState, Type](Error.ProjNotRecordType)
        }
      } yield fieldType
    case TermMethodProj(ty, method) =>
      for {
        tyT1 <- pureTypeOf(ty)
        tyT1S <- EitherT.liftF(simplifyType(tyT1))
        fieldType <- tyT1S match {
          case TypeRec(_, _, _) =>
            Context.getMethodType(findRootTypeIndex(tyT1).get, method)
          case _ =>
            EitherT.leftT[ContextState, Type](Error.MethodProjNotDataType)
        }
      } yield fieldType
    case TermFix(t1) =>
      for {
        tyT1 <- pureTypeOf(t1)
        tyT1S <- EitherT.liftF(simplifyType(tyT1))
        bodyType <- tyT1S match {
          case TypeArrow(tyT11, tyT12) =>
            EitherT(
              isTypeEqual(tyT12, tyT11).map(
                Either.cond(_, tyT12, Error.FixWrongBodyType)
              )
            )
          case _ => EitherT.leftT[ContextState, Type](Error.FixNotArrowType)
        }
      } yield bodyType
    case TermRecord(fields) =>
      fields
        .traverse { case (v, term) =>
          pureTypeOf(term).map((v, _))
        }
        .map(TypeRecord(_))
    case TermFold(tyS) =>
      EitherT(simplifyType(tyS).map(_ match {
        case TypeRec(_, _, tyT) =>
          Right(TypeArrow(typeSubstituteTop(tyS, tyT), tyS))
        case _ => Left(Error.FoldNotRecursiveType)
      }))
    case TermLet(variable, t1, t2) =>
      for {
        tyT1 <- pureTypeOf(t1)
        _ <- EitherT.liftF(Context.addBinding(variable, VarBind(tyT1)))
        tyT2 <- pureTypeOf(t2)
      } yield typeShift(-1, tyT2)
    case TermTAbs(v, t) =>
      for {
        _ <- EitherT.liftF(Context.addBinding(v, TypeVarBind(KindStar)))
        ty <- pureTypeOf(t).map(TypeAll(v, KindStar, _))
      } yield ty
    case TermTApp(t1, ty2) =>
      for {
        k2 <- kindOf(ty2)
        ty1 <- pureTypeOf(t1)
        tyT1 <- EitherT.liftF(simplifyType(ty1))
        ty <- tyT1 match {
          case TypeAll(_, k1, tyT2) =>
            EitherT.fromEither[ContextState](
              Either.cond(
                k1 == k2,
                typeSubstituteTop(ty2, tyT2),
                Error.TAppKindMismatch
              )
            )
          case _ => EitherT.leftT[ContextState, Type](Error.TAppNotAllType)
        }
      } yield ty
    case TermMatch(exprTerm, cases) =>
      for {
        // Get the type of the expression to match.
        ty1 <- pureTypeOf(exprTerm)
        exprType <- EitherT.liftF(unfoldType(ty1))
        // Get the types of expression for each case, with a check for pattern
        // type equivalence to the expression type.
        caseExprTypes <- cases.traverse { (v) =>
          Context.runE(
            for {
              patternTypeInfo <- typeOfPattern(v._1, exprType)
              _ <- patternTypeInfo._1
                .map(ty =>
                  EitherT(
                    isTypeEqual(exprType, ty).map(
                      Either.cond(
                        _,
                        (),
                        Error.MatchPatternTypeMismatch
                      )
                    )
                  )
                )
                .getOrElse(EitherT.pure[ContextState, Error](()))
              caseExprType <- pureTypeOf(v._2)
            } yield typeShift(-patternTypeInfo._2, caseExprType)
          )
        }
        // Finally, check if all the case expressions have the same type.
        caseExprType <- EitherT(
          caseExprTypes
            .traverse(isTypeEqual(_, caseExprTypes.head))
            .map(l =>
              Either.cond(
                l.forall(identity),
                caseExprTypes.head,
                Error.MatchCasesTypeMismatch
              )
            )
        )
      } yield caseExprType
    case TermTag(tag, t1, ty1) =>
      for {
        tyS <- EitherT.liftF(simplifyType(ty1))
        tySVar <- EitherT.fromEither[ContextState](tyS match {
          case TypeRec(_, _, v @ TypeVariant(_)) => Right(v)
          case _                                 => Left(Error.TagNotVariantType)
        })
        tyTiExpected <- EitherT.fromEither[ContextState](
          tySVar.v
            .find(_._1 == tag)
            .map { case (_, ty) => typeSubstituteTop(findRootType(ty1), ty) }
            .toRight(Error.TagVariantFieldNotFound(tag))
        )
        tyTi <- pureTypeOf(t1)
        ty <- EitherT(
          isTypeEqual(tyTi, tyTiExpected).map(
            Either.cond(
              _,
              ty1,
              Error.TagFieldTypeMismatch
            )
          )
        )
      } yield typeSubstituteTop(ty1, tySVar)
    case TermTrue        => (TypeBool: Type).pure[StateEither]
    case TermFalse       => (TypeBool: Type).pure[StateEither]
    case TermInt(_)      => (TypeInt: Type).pure[StateEither]
    case TermFloat(_)    => (TypeFloat: Type).pure[StateEither]
    case TermString(_)   => (TypeString: Type).pure[StateEither]
    case TermUnit        => (TypeUnit: Type).pure[StateEither]
    case TermBuiltin(ty) => ty.pure[StateEither]

  }

  def typeOfPattern(p: Pattern, expr: Type): StateEither[(Option[Type], Int)] =
    p match {
      case t: Term        => typeOf(t).map(v => (Some(v), 0))
      case PatternDefault => EitherT.pure((None, 0))
      case PatternNode(node, vars) =>
        expr match {
          case TypeVariant(fields) =>
            for {
              ty <- EitherT.fromEither[ContextState](
                fields
                  .find(_._1 == node)
                  .map(_._2)
                  .toRight(Error.MatchPatternNotInVariant(node))
              )
              numOfBindVars <- ty match {
                case tyR @ TypeRecord(_) => typeOfPattern(p, tyR).map(_._2)
                case _                   => 0.pure[StateEither]
              }
            } yield (Some(expr), numOfBindVars)
          case TypeRecord(fields) =>
            for {
              idx <- EitherT(
                State.inspect((ctx: Context) =>
                  Context
                    .nameToIndex(ctx, node)
                    .toRight(Error.MatchPatternRecordNotFound(node))
                )
              )
              _ <- Context.getType(idx)
              _ <- bindFieldsToVars(fields, vars)
            } yield (Some(expr), fields.length)
          case _ =>
            EitherT.leftT(Error.MatchPatternTypeMismatch)
        }
    }

  def bindFieldsToVars(
      fields: List[(String, Type)],
      vars: List[String]
  ): StateEither[List[String]] =
    fields.length == vars.length match {
      case true =>
        EitherT.right(
          fields.zipWithIndex.traverse { case ((f, i)) =>
            Context.addBinding(f._1, VarBind(typeShift(i, f._2)))
          }
        )
      case false =>
        EitherT.leftT(Error.MatchPatternWrongVariables)
    }

  def unfoldType(tyS: Type): ContextState[Type] = {
    simplifyType(tyS).map(_ match {
      case TypeRec(_, _, tyT) =>
        typeSubstituteTop(findRootType(tyS), tyT)
      case ty => ty
    })
  }

  def isTypeEqual(ty1: Type, ty2: Type): ContextState[Boolean] = Context.run {
    val tys = for {
      ty1S <- simplifyType(ty1)
      ty2S <- simplifyType(ty2)
    } yield (ty1S, ty2S)
    tys.flatMap { case (tyS, tyT) =>
      (tyS, tyT) match {
        case (TypeString, TypeString)   => true.pure[ContextState]
        case (TypeId(id1), TypeId(id2)) => (id1 == id2).pure[ContextState]
        case (TypeUnit, TypeUnit)       => true.pure[ContextState]
        case (TypeFloat, TypeFloat)     => true.pure[ContextState]
        case (TypeInt, TypeInt)         => true.pure[ContextState]
        case (TypeBool, TypeBool)       => true.pure[ContextState]
        case (TypeArrow(tyS1, tyS2), TypeArrow(tyT1, tyT2)) =>
          for {
            b1 <- isTypeEqual(tyS1, tyT1)
            b2 <- isTypeEqual(tyS2, tyT2)
          } yield b1 && b2
        case (TypeRec(x1, k1, tyS1), TypeRec(_, k2, tyT1)) if k1 == k2 =>
          Context.addName(x1).flatMap(_ => isTypeEqual(tyS1, tyT1))
        case (TypeVar(idx1, _), TypeVar(idx2, _)) =>
          (idx1 == idx2).pure[ContextState]
        case (TypeVar(idx, _), _) =>
          getTypeAbb(idx).semiflatMap(isTypeEqual(_, tyT)).getOrElse(false)
        case (_, TypeVar(idx, _)) =>
          getTypeAbb(idx).semiflatMap(isTypeEqual(tyS, _)).getOrElse(false)
        case (TypeRecord(f1), TypeRecord(f2)) if f1.length == f2.length =>
          f1.traverse { case (l1, tyT1) =>
            f2.find(_._1 == l1)
              .map(f => isTypeEqual(tyT1, f._2))
              .getOrElse(false.pure[ContextState])
          }.map(_.forall(identity))
        case (TypeVariant(f1), TypeVariant(f2)) if f1.length == f2.length =>
          f1.traverse { case (l1, tyT1) =>
            f2.find(_._1 == l1)
              .map(f => isTypeEqual(tyT1, f._2).map(_ && l1 == f._1))
              .getOrElse(false.pure[ContextState])
          }.map(_.forall(identity))
        case (TypeAll(tyX1, k1, tyS1), TypeAll(_, k2, tyT1)) if k1 == k2 =>
          Context.addName(tyX1).flatMap(_ => isTypeEqual(tyS1, tyT1))
        case (TypeAbs(tyX1, tyS1), TypeAbs(_, tyT1)) =>
          Context.addName(tyX1).flatMap(_ => isTypeEqual(tyS1, tyT1))
        case (TypeApp(tyS1, tyS2), TypeApp(tyT1, tyT2)) =>
          for {
            b1 <- isTypeEqual(tyS1, tyT1)
            b2 <- isTypeEqual(tyS2, tyT2)
          } yield b1 && b2
        case _ => false.pure[ContextState]
      }
    }
  }

  def simplifyType(ty: Type): ContextState[Type] =
    for {
      tyT <- ty match {
        case TypeApp(tyT1, tyT2) => simplifyType(tyT1).map(TypeApp(_, tyT2))
        case _                   => ty.pure[ContextState]
      }
      tyT1 <- computeType(tyT).semiflatMap(simplifyType(_)).getOrElse(tyT)
    } yield tyT1

  def computeType(ty: Type): StateOption[Type] = ty match {
    case TypeVar(idx, _) => getTypeAbb(idx)
    case TypeApp(TypeAbs(_, tyT12), tyT2) =>
      OptionT.some(typeSubstituteTop(tyT2, tyT12))
    case _ => OptionT.none
  }

  @tailrec
  def findRootType(ty: Type): Type = ty match {
    case v @ TypeVar(_, _) => v
    case TypeApp(ty1, ty2) => findRootType(ty1)
    case _                 => ty
  }

  @tailrec
  def findRootTypeIndex(ty: Type): Option[Int] = ty match {
    case TypeVar(index, _) => Some(index)
    case TypeApp(ty1, ty2) => findRootTypeIndex(ty1)
    case _                 => None
  }

  def getTypeAbb(index: Int): StateOption[Type] =
    Context
      .getBinding(index)
      .toOption
      .flatMap(_ match {
        case TypeAbbBind(ty, _) => OptionT.some(ty)
        case _                  => OptionT.none
      })

  def isTypeAbb(index: Int): ContextState[Boolean] = Context
    .getBinding(index)
    .toOption
    .map(_ match {
      case TypeAbbBind(_, _) => true
      case _                 => false
    })
    .getOrElse(false)

  def pureKindOf(t: Type): StateEither[Kind] = Context.runE(kindOf(t))

  def kindOf(t: Type): StateEither[Kind] = t match {
    case TypeArrow(ty1, ty2) =>
      for {
        _ <- checkKindStar(ty1)
        _ <- checkKindStar(ty1)
      } yield KindStar
    case TypeVar(idx, _) => getKind(idx)
    case TypeRecord(fields) =>
      fields
        .traverse(f => checkKindStar(f._2))
        .map(_ => KindStar)
    case TypeAll(tyX, k1, tyT2) =>
      for {
        _ <- EitherT.liftF(Context.addBinding(tyX, TypeVarBind(k1)))
        _ <- checkKindStar(tyT2)
      } yield KindStar
    case TypeAbs(tyX, tyT2) =>
      for {
        _ <- EitherT.liftF(addBinding(tyX, TypeVarBind(KindStar)))
        k <- pureKindOf(tyT2)
      } yield KindArrow(KindStar, k)
    case TypeApp(tyT1, tyT2) =>
      for {
        k1 <- pureKindOf(tyT1)
        k2 <- pureKindOf(tyT2)
        k <- EitherT.fromEither[ContextState](k1 match {
          case KindArrow(k11, k12) =>
            Either.cond(k11 == k2, k12, Error.KindAppParameterMismatch)
          case _ => Left(Error.NotArrowKind)
        })
      } yield k
    case TypeRec(tyX, k1, tyT2) =>
      for {
        _ <- EitherT.liftF(addBinding(tyX, TypeVarBind(k1)))
        k <- pureKindOf(tyT2)
      } yield k
    case _ => EitherT.rightT(KindStar)
  }

  def checkKindStar(ty: Type): StateEither[Kind] =
    pureKindOf(ty).flatMap(k =>
      EitherT.cond(k == KindStar, KindStar, Error.KindStarExpected)
    )

  def getKind(idx: Int): StateEither[Kind] =
    Context
      .getBinding(idx)
      .flatMap(_ match {
        case TypeVarBind(k)          => EitherT.rightT(k)
        case TypeAbbBind(_, Some(k)) => EitherT.rightT(k)
        case TypeAbbBind(_, None)    => EitherT.left(Error.NoKindForVariable(idx))
        case _                       => EitherT.left(Error.WrongKindBindingForVariable(idx))
      })

  object Error {
    // Type Errors
    val AscribeWrongType = "Body of as-term does not have the expected type."
    val AppParameterTypeMismatch = "Parameter type mismatch."
    val AppNotArrowType = "Arrow type expected on application."
    def ProjFieldNotFound(f: String) = s"Field $f not found on the record."
    val ProjNotRecordType = "Expected record type."
    val MethodProjNotDataType = "Expected data type."
    val FixWrongBodyType = "Type of body not compatible with the domain."
    val FixNotArrowType = "Arrow type expected for fix combinator."
    val FoldNotRecursiveType = "Recursive type expected on fold."
    val UnfoldNotRecursiveType = "Recursive type expected on unfold."
    val TAppKindMismatch = "Type argument has wrong kind."
    val TAppNotAllType = "Universal type expected on type application."
    val MatchPatternTypeMismatch =
      "Pattern type is not the same as the type of the match expression."
    val MatchCasesTypeMismatch = "Type between cases don't match."
    def MatchPatternNotInVariant(p: String) =
      s"Pattern $p not found in type variant."
    def MatchPatternRecordNotFound(p: String) =
      s"Pattern $p not found for type record."
    def MatchPatternWrongVariables =
      "Pattern has wrong number of variables for the type."
    val TagNotVariantType = "Expected variant type on the tag."
    def TagVariantFieldNotFound(f: String) =
      s"Field $f not found on the variant."
    val TagFieldTypeMismatch = "Field does not have an expected type."

    // Kind Errors
    val KindStarExpected = "Kind * expected."
    def NoKindForVariable(idx: Int): ContextState[String] = State.inspect {
      ctx =>
        s"No kind recorded for type ${Context.indexToName(ctx, idx).get}"
    }
    def WrongKindBindingForVariable(idx: Int): ContextState[String] =
      State.inspect { ctx =>
        s"getKind: Wrong kind of binding for variable ${Context.indexToName(ctx, idx).get}"
      }
    val KindAppParameterMismatch = "Parameter kind mismatch."
    val NotArrowKind = "Arrow kind expected"
  }

}
