package core

import cats.data.EitherT
import cats.data.OptionT
import cats.data.State
import cats.data.StateT
import cats.implicits._
import core.Bindings._
import core.Context._
import core.Shifting._
import core.Terms._
import core.Types._
import parser.FuseParser._
import parser.Info._
import parser.Info.ShowInfo._

import scala.annotation.tailrec
import scala.util._

object TypeChecker {
  val MainFunction = "main"

  def run(binds: List[Bind]): Either[Error, List[Bind]] =
    check(binds).value.runEmptyA.value

  def check(binds: List[Bind]): StateEither[List[Bind]] =
    binds
      .traverse(bind =>
        for {
          binding <- checkBinding(bind.b)
          id <- EitherT.liftF[ContextState, Error, String](
            addBinding(bind.i, binding)
          )
        } yield Bind(id, binding)
      )
      .flatMap(binds =>
        binds.exists { bind => bind.i == MainFunction } match {
          case true  => binds.pure[StateEither]
          case false => TypeError.format(MainFunctionNotFoundTypeError())
        }
      )

  def checkBinding(b: Binding): StateEither[Binding] = b match {
    case NameBind           => EitherT.rightT(NameBind)
    case t @ TypeVarBind(_) => EitherT.rightT(t)
    case v @ VarBind(_)     => EitherT.rightT(v)
    case TypeAbbBind(ty, None) =>
      pureKindOf(ty).map(k => TypeAbbBind(ty, Some(k)))
    case TermAbbBind(term, None) =>
      pureTypeOf(term).map(ty => TermAbbBind(term, Some(ty)))
    case _ => EitherT.rightT(b)

  }

  def pureTypeOf(t: Term): StateEither[Type] = Context.runE(typeOf(t))

  def typeOf(term: Term): StateEither[Type] = term match {
    case TermAscribe(info, t1, typeToAscribe) =>
      for {
        _ <- checkKindStar(typeToAscribe)
        tyT1 <- pureTypeOf(t1)
        t <- isTypeEqualWithTypeError(
          tyT1,
          typeToAscribe,
          AscribeWrongTypeError(
            info,
            tyT1,
            typeToAscribe
          )
        )
      } yield t
    case TermVar(info, idx, _) => Context.getType(info, idx)
    case TermClosure(info, variable, Some(variableType), expr) =>
      for {
        _ <- checkKindStar(variableType)
        _ <- EitherT.liftF(Context.addBinding(variable, VarBind(variableType)))
        tyT2 <- pureTypeOf(expr)
      } yield TypeArrow(info, variableType, typeShift(-1, tyT2))
    case TermClosure(info, _, None, _) =>
      throw new Exception("Closure without annotation not supported.")
    case TermAbs(info, variable, variableType, expr, returnType) =>
      for {
        _ <- checkKindStar(variableType)
        _ <- EitherT.liftF(Context.addBinding(variable, VarBind(variableType)))
        exprType <- pureTypeOf(expr)
        _ <- returnType match {
          case Some(ty) =>
            isTypeEqualWithTypeError(
              exprType,
              ty,
              WrongReturnTypeError(ty.info, exprType, ty)
            )
          case _ => ().pure[StateEither]
        }
      } yield TypeArrow(info, variableType, typeShift(-1, exprType))
    case TermApp(info, t1, t2) =>
      for {
        tyT1 <- pureTypeOf(t1)
        tyT2 <- pureTypeOf(t2)
        tyT1S <- EitherT.liftF(simplifyType(tyT1))
        ty <- tyT1S match {
          case TypeArrow(_, tyT11, tyT12) =>
            isTypeEqualWithTypeError(
              tyT2,
              tyT11,
              MismatchFunctionTypeError(
                t2.info,
                tyT2,
                tyT11
              )
            ).map(_ => tyT12)
          case _ =>
            TypeError.format(
              VariableNotFunctionTypeError(
                t1.info,
                tyT1
              )
            )
        }
      } yield ty
    case TermProj(info, ty, label) =>
      for {
        tyT1 <- pureTypeOf(ty)
        tyT1S <- EitherT.liftF(simplifyType(tyT1))
        fieldType <- tyT1S match {
          case TypeRec(_, _, _, TypeRecord(_, fields)) =>
            fields
              .find { case (f, _) => f == label }
              .map { case (_, ty) => ty.pure[StateEither] }
              .getOrElse(
                TypeError.format(
                  FieldNotFoundTypeError(info, tyT1, label)
                )
              )
          case _ => TypeError.format(NoFieldsOnTypeError(info, tyT1))
        }
      } yield fieldType
    case TermMethodProj(info, term, method) =>
      for {
        tyT1 <- pureTypeOf(term)
        tyT1S <- EitherT.liftF(simplifyType(tyT1))
        rootTypeIndexOption = findRootTypeIndex(tyT1)
        fieldType <- (tyT1S, rootTypeIndexOption) match {
          case (TypeRec(_, _, _, _), Some(rootTypeIndex)) =>
            for {
              optionName <- EitherT.liftF(
                State.inspect { (ctx: Context) =>
                  indexToName(ctx, rootTypeIndex)
                }
              )
              typeName <- optionName match {
                case Some(name) => name.pure[StateEither]
                case None       => TypeError.format(NotFoundTypeError(info))
              }
              methodOptionIdx <- EitherT.liftF(
                State.inspect { (ctx: Context) =>
                  nameToIndex(ctx, Desugar.toMethodId(method, typeName))
                }
              )
              methodIdx <- methodOptionIdx match {
                case Some(idx) => idx.pure[StateEither]
                case None =>
                  TypeError.format(
                    MethodNotFoundTypeError(info, tyT1, method)
                  )
              }
              methodType <- getType(info, methodIdx)
            } yield methodType
          case _ => TypeError.format(NoMethodsOnTypeError(info, tyT1S))
        }
      } yield fieldType
    case TermFix(info, t1) =>
      for {
        tyT1 <- pureTypeOf(t1)
        tyT1S <- EitherT.liftF(simplifyType(tyT1))
        bodyType <- tyT1S match {
          case TypeArrow(_, tyT11, tyT12) =>
            isTypeEqualWithTypeError(
              tyT12,
              tyT11,
              WrongReturnTypeError(
                info,
                tyT11,
                tyT12
              )
            )
          case _ =>
            TypeError.format(InvalidFunctionTypeError(info, tyT1S))
        }
      } yield bodyType
    case TermRecord(info, fields) =>
      fields
        .traverse { case (v, term) =>
          pureTypeOf(term).map((v, _))
        }
        .map(TypeRecord(info, _))
    case TermFold(info, tyS) =>
      EitherT
        .liftF(simplifyType(tyS))
        .flatMap(_ match {
          case TypeRec(_, _, _, tyT) =>
            (TypeArrow(info, typeSubstituteTop(tyS, tyT), tyS): Type)
              .pure[StateEither]
          case _ =>
            TypeError.format(InvalidFoldForRecursiveTypeError(info, tyS))
        })
    case TermLet(_, variable, t1, t2) =>
      for {
        tyT1 <- pureTypeOf(t1)
        _ <- EitherT.liftF(Context.addBinding(variable, VarBind(tyT1)))
        tyT2 <- pureTypeOf(t2)
      } yield typeShift(-1, tyT2)
    case TermTAbs(info, v, t) =>
      for {
        _ <- EitherT.liftF(Context.addBinding(v, TypeVarBind(KindStar)))
        ty <- pureTypeOf(t).map(TypeAll(info, v, KindStar, _))
      } yield ty
    case TermTApp(info, t1, ty2) =>
      for {
        k2 <- kindOf(ty2)
        ty1 <- pureTypeOf(t1)
        tyT1 <- EitherT.liftF(simplifyType(ty1))
        ty <- tyT1 match {
          case TypeAll(_, _, k1, tyT2) =>
            typeErrorEitherCond(
              k1 == k2,
              typeSubstituteTop(ty2, tyT2),
              InvalidTypeArgumentTypeError(ty2.info, ty2)
            )
          case _ =>
            TypeError.format(
              TypeArgumentsNotAllowedTypeError(
                ty2.info,
                tyT1
              )
            )
        }
      } yield ty
    case TermMatch(info, exprTerm, cases) =>
      for {
        // Get the type of the expression to match.
        ty1 <- pureTypeOf(exprTerm)
        exprType <- EitherT.liftF(unfoldType(ty1))
        // Get the types of expression for each case, with a check for pattern
        // type equivalence to the expression type.
        caseExprTypes <- cases.traverse { (v) =>
          Context.runE(
            for {
              patternTypeInfo <- typeOfPattern(v._1, exprType, ty1)
              _ <- patternTypeInfo._1
                .map(ty =>
                  isTypeEqualWithTypeError(
                    exprType,
                    ty,
                    MatchTypeMismatchPatternTypeError(
                      v._1.info,
                      ty1,
                      ty
                    )
                  )
                )
                .getOrElse(EitherT.pure[ContextState, Error](()))
              caseExprType <- pureTypeOf(v._2)
            } yield (typeShift(-patternTypeInfo._2, caseExprType), v._2.info)
          )
        }
        // Finally, check if all the case expressions have the same type.
        caseExprType <- caseExprTypes
          .traverse { case (caseExprType, info) =>
            isTypeEqualWithTypeError(
              caseExprType,
              caseExprTypes.head._1,
              MatchCasesTypeError(
                info,
                caseExprType,
                caseExprTypes.head._1
              )
            )
          }
          .map(_.head)
      } yield caseExprType
    case TermTag(info, tag, t1, ty1) =>
      for {
        tyS <- EitherT.liftF(simplifyType(ty1))
        tySVar <- tyS match {
          case TypeRec(_, _, _, v @ TypeVariant(_, _)) => v.pure[StateEither]
          case _ =>
            TypeError.format(TagNotVariantTypeError(info, ty1))
        }
        tyTiExpected <- tySVar.v
          .find(_._1 == tag)
          .map { case (_, ty) =>
            typeSubstituteTop(findRootType(ty1), ty).pure[StateEither]
          }
          .getOrElse(
            TypeError.format(
              TagVariantFieldNotFoundTypeError(info, ty1, tag)
            )
          )
        tyTi <- pureTypeOf(t1)
        _ <- isTypeEqualWithTypeError(
          tyTi,
          tyTiExpected,
          TagFieldMismatchTypeError(info, tyTi, tyTiExpected)
        )
      } yield typeSubstituteTop(ty1, tySVar)
    case TermTrue(info)      => EitherT.rightT(TypeBool(info))
    case TermFalse(info)     => EitherT.rightT(TypeBool(info))
    case TermInt(info, _)    => EitherT.rightT(TypeInt(info))
    case TermFloat(info, _)  => EitherT.rightT(TypeFloat(info))
    case TermString(info, _) => EitherT.rightT(TypeString(info))
    case TermUnit(info)      => EitherT.rightT(TypeUnit(info))
    case TermBuiltin(ty)     => ty.pure[StateEither]
  }

  def isTypeEqualWithTypeError(
      ty1: Type,
      ty2: Type,
      error: TypeError
  ): StateEither[Type] = for {
    isEqual <- EitherT.liftF(isTypeEqual(ty1, ty2))
    t <- typeErrorEitherCond(
      isEqual,
      ty1,
      error
    )
  } yield t

  def typeErrorEitherCond[T](
      cond: Boolean,
      value: T,
      error: TypeError
  ): StateEither[T] =
    cond match {
      case true  => value.pure[StateEither]
      case false => TypeError.format(error)
    }

  def typeOfPattern(
      p: Pattern,
      unfoldedMatchExprType: Type,
      matchExprType: Type
  ): StateEither[(Option[Type], Int)] =
    p match {
      case t: Term           => typeOf(t).map(v => (Some(v), 0))
      case PatternDefault(_) => EitherT.pure((None, 0))
      case PatternNode(info, node, vars) =>
        unfoldedMatchExprType match {
          case TypeVariant(_, fields) =>
            for {
              ty <- fields
                .find(_._1 == node)
                .map { case (_, v) => v.pure[StateEither] }
                .getOrElse(
                  TypeError.format(
                    MatchVariantPatternMismatchTypeError(
                      info,
                      matchExprType,
                      node
                    )
                  )
                )
              numOfBindVars <- ty match {
                case tyR @ TypeRecord(_, _) =>
                  typeOfPattern(p, tyR, matchExprType).map(_._2)
                case _ => 0.pure[StateEither]
              }
            } yield (Some(unfoldedMatchExprType), numOfBindVars)
          case TypeRecord(_, fields) =>
            for {
              optionIdx <- EitherT.liftF(
                State.inspect((ctx: Context) =>
                  Context
                    .nameToIndex(ctx, node)
                )
              )
              idx <- optionIdx
                .map(_.pure[StateEither])
                .getOrElse(
                  TypeError.format(
                    MatchRecordPatternMismatchTypeError(
                      info,
                      matchExprType,
                      node
                    )
                  )
                )
              _ <- Context.getType(info, idx)
              _ <- bindFieldsToVars(info, fields, vars, unfoldedMatchExprType)
            } yield (Some(unfoldedMatchExprType), fields.length)
          case _ =>
            TypeError.format(
              MatchExprNotDataTypeError(info, unfoldedMatchExprType, node)
            )
        }
    }

  def bindFieldsToVars(
      info: Info,
      fields: List[(String, Type)],
      vars: List[String],
      exprType: Type
  ): StateEither[List[String]] =
    fields.length == vars.length match {
      case true =>
        EitherT.right(
          fields.zipWithIndex.zip(vars).traverse { case ((f, i), v) =>
            Context.addBinding(v, VarBind(typeShift(i, f._2)))
          }
        )
      case false =>
        TypeError.format(
          MatchPatternWrongVarsTypeError(
            info,
            exprType,
            fields.length,
            vars.length
          )
        )
    }

  def unfoldType(tyS: Type): ContextState[Type] = {
    simplifyType(tyS).map(_ match {
      case TypeRec(_, _, _, tyT) =>
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
        case (TypeString(_), TypeString(_))   => true.pure[ContextState]
        case (TypeId(_, id1), TypeId(_, id2)) => (id1 == id2).pure[ContextState]
        case (TypeUnit(_), TypeUnit(_))       => true.pure[ContextState]
        case (TypeFloat(_), TypeFloat(_))     => true.pure[ContextState]
        case (TypeInt(_), TypeInt(_))         => true.pure[ContextState]
        case (TypeBool(_), TypeBool(_))       => true.pure[ContextState]
        case (TypeArrow(_, tyS1, tyS2), TypeArrow(_, tyT1, tyT2)) =>
          for {
            b1 <- isTypeEqual(tyS1, tyT1)
            b2 <- isTypeEqual(tyS2, tyT2)
          } yield b1 && b2
        case (TypeRec(_, x1, k1, tyS1), TypeRec(_, _, k2, tyT1)) if k1 == k2 =>
          Context.addName(x1).flatMap(_ => isTypeEqual(tyS1, tyT1))
        case (TypeVar(_, idx1, _), TypeVar(_, idx2, _)) =>
          (idx1 == idx2).pure[ContextState]
        case (TypeVar(_, idx, _), _) =>
          getTypeAbb(idx).semiflatMap(isTypeEqual(_, tyT)).getOrElse(false)
        case (_, TypeVar(_, idx, _)) =>
          getTypeAbb(idx).semiflatMap(isTypeEqual(tyS, _)).getOrElse(false)
        case (TypeRecord(_, f1), TypeRecord(_, f2)) if f1.length == f2.length =>
          f1.traverse { case (l1, tyT1) =>
            f2.find(_._1 == l1)
              .map(f => isTypeEqual(tyT1, f._2))
              .getOrElse(false.pure[ContextState])
          }.map(_.forall(identity))
        case (TypeVariant(_, f1), TypeVariant(_, f2))
            if f1.length == f2.length =>
          f1.traverse { case (l1, tyT1) =>
            f2.find(_._1 == l1)
              .map(f => isTypeEqual(tyT1, f._2).map(_ && l1 == f._1))
              .getOrElse(false.pure[ContextState])
          }.map(_.forall(identity))
        case (TypeAll(_, tyX1, k1, tyS1), TypeAll(_, _, k2, tyT1))
            if k1 == k2 =>
          Context.addName(tyX1).flatMap(_ => isTypeEqual(tyS1, tyT1))
        case (TypeAbs(_, tyX1, tyS1), TypeAbs(_, _, tyT1)) =>
          Context.addName(tyX1).flatMap(_ => isTypeEqual(tyS1, tyT1))
        case (TypeApp(_, tyS1, tyS2), TypeApp(_, tyT1, tyT2)) =>
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
        case TypeApp(info, tyT1, tyT2) =>
          simplifyType(tyT1).map(TypeApp(info, _, tyT2))
        case _ => ty.pure[ContextState]
      }
      tyT1 <- computeType(tyT).semiflatMap(simplifyType(_)).getOrElse(tyT)
    } yield tyT1

  def computeType(ty: Type): StateOption[Type] = ty match {
    case TypeVar(_, idx, _) => getTypeAbb(idx)
    case TypeApp(_, TypeAbs(_, _, tyT12), tyT2) =>
      OptionT.some(typeSubstituteTop(tyT2, tyT12))
    case _ => OptionT.none
  }

  @tailrec
  def findRootType(ty: Type): Type = ty match {
    case v @ TypeVar(_, _, _) => v
    case TypeApp(_, ty1, ty2) => findRootType(ty1)
    case _                    => ty
  }

  @tailrec
  def findRootTypeIndex(ty: Type): Option[Int] = ty match {
    case TypeVar(_, index, _) => Some(index)
    case TypeApp(_, ty1, ty2) => findRootTypeIndex(ty1)
    case _                    => None
  }

  def getTypeAbb(index: Int): StateOption[Type] =
    Context
      .getBinding(UnknownInfo, index)
      .toOption
      .flatMap(_ match {
        case TypeAbbBind(ty, _) => OptionT.some(ty)
        case _                  => OptionT.none
      })

  def isTypeAbb(index: Int): ContextState[Boolean] = Context
    .getBinding(UnknownInfo, index)
    .toOption
    .map(_ match {
      case TypeAbbBind(_, _) => true
      case _                 => false
    })
    .getOrElse(false)

  def pureKindOf(t: Type): StateEither[Kind] = Context.runE(kindOf(t))

  def kindOf(t: Type): StateEither[Kind] = t match {
    case TypeArrow(_, ty1, ty2) =>
      for {
        _ <- checkKindStar(ty1)
        _ <- checkKindStar(ty1)
      } yield KindStar
    case TypeVar(info, idx, _) => getKind(info, idx)
    case TypeRecord(_, fields) =>
      fields
        .traverse(f => checkKindStar(f._2))
        .map(_ => KindStar)
    case TypeAll(_, tyX, k1, tyT2) =>
      for {
        _ <- EitherT.liftF(Context.addBinding(tyX, TypeVarBind(k1)))
        _ <- checkKindStar(tyT2)
      } yield KindStar
    case TypeAbs(_, tyX, tyT2) =>
      for {
        _ <- EitherT.liftF(addBinding(tyX, TypeVarBind(KindStar)))
        k <- pureKindOf(tyT2)
      } yield KindArrow(KindStar, k)
    case TypeApp(info, tyT1, tyT2) =>
      for {
        k1 <- pureKindOf(tyT1)
        k2 <- pureKindOf(tyT2)
        k <- k1 match {
          case KindArrow(k11, k12) =>
            typeErrorEitherCond(
              k11 == k2,
              k12,
              KindParameterMismatchTypeError(
                info,
                tyT2,
                k2,
                k11
              )
            )
          case _ => TypeError.format(NoTypeArgumentsTypeError(info, tyT1))
        }
      } yield k
    case TypeRec(_, tyX, k1, tyT2) =>
      for {
        _ <- EitherT.liftF(addBinding(tyX, TypeVarBind(k1)))
        k <- pureKindOf(tyT2)
      } yield k
    case _ => EitherT.rightT(KindStar)
  }

  def checkKindStar(ty: Type): StateEither[Kind] =
    pureKindOf(ty).flatMap { k =>
      typeErrorEitherCond(
        k == KindStar,
        KindStar,
        InvalidTypeArgumentTypeError(ty.info, ty)
      )
    }

  def getKind(info: Info, idx: Int): StateEither[Kind] =
    Context
      .getBinding(info, idx)
      .flatMap(_ match {
        case TypeVarBind(k)          => EitherT.rightT(k)
        case TypeAbbBind(_, Some(k)) => EitherT.rightT(k)
        case TypeAbbBind(_, None) =>
          TypeError.format(NoKindForTypeError(info, idx))
        case _ => TypeError.format(BindingNotFoundTypeError(info))
      })
}
