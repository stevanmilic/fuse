package core

import cats.data.EitherT
import cats.data.OptionT
import cats.data.State
import cats.data.StateT
import cats.implicits._
import core.Context._
import core.Shifting._
import parser.FuseParser._
import parser.Info._

import scala.annotation.tailrec
import scala.util._

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
    case TermClosure(_, variable, Some(variableType), expr) =>
      for {
        _ <- checkKindStar(variableType)
        _ <- EitherT.liftF(Context.addBinding(variable, VarBind(variableType)))
        tyT2 <- pureTypeOf(expr)
      } yield TypeArrow(variableType, typeShift(-1, tyT2))
    case TermClosure(info, _, None, _) =>
      throw new Exception("Closure without annotation not supported.")
    case TermAbs(_, variable, variableType, expr, _) =>
      for {
        _ <- checkKindStar(variableType)
        _ <- EitherT.liftF(Context.addBinding(variable, VarBind(variableType)))
        tyT2 <- pureTypeOf(expr)
      } yield TypeArrow(variableType, typeShift(-1, tyT2))
    case TermApp(info, t1, t2) =>
      for {
        tyT1 <- pureTypeOf(t1)
        tyT2 <- pureTypeOf(t2)
        tyT1S <- EitherT.liftF(simplifyType(tyT1))
        ty <- tyT1S match {
          case TypeArrow(tyT11, tyT12) =>
            isTypeEqualWithTypeError(
              tyT2,
              tyT11,
              MismatchFunctionTypeError(
                info,
                tyT2,
                tyT11
              )
            ).map(_ => tyT12)
          case _ =>
            TypeError.format(
              VariableNotFunctionTypeError(
                info,
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
          case TypeRec(_, _, TypeRecord(fields)) =>
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
          case (TypeRec(_, _, _), Some(rootTypeIndex)) =>
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
          case TypeArrow(tyT11, tyT12) =>
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
    case TermRecord(_, fields) =>
      fields
        .traverse { case (v, term) =>
          pureTypeOf(term).map((v, _))
        }
        .map(TypeRecord(_))
    case TermFold(info, tyS) =>
      EitherT
        .liftF(simplifyType(tyS))
        .flatMap(_ match {
          case TypeRec(_, _, tyT) =>
            (TypeArrow(typeSubstituteTop(tyS, tyT), tyS): Type)
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
    case TermTAbs(_, v, t) =>
      for {
        _ <- EitherT.liftF(Context.addBinding(v, TypeVarBind(KindStar)))
        ty <- pureTypeOf(t).map(TypeAll(v, KindStar, _))
      } yield ty
    case TermTApp(info, t1, ty2) =>
      for {
        k2 <- kindOf(ty2)
        ty1 <- pureTypeOf(t1)
        tyT1 <- EitherT.liftF(simplifyType(ty1))
        ty <- tyT1 match {
          case TypeAll(_, k1, tyT2) =>
            typeErrorEitherCond(
              k1 == k2,
              typeSubstituteTop(ty2, tyT2),
              InvalidTypeArgumentTypeError(info, ty2)
            )
          case _ =>
            TypeError.format(
              TypeArgumentsNotAllowedTypeError(
                info,
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
                      // v._2.info,
                      UnknownInfo,
                      ty1,
                      ty
                    )
                  )
                )
                .getOrElse(EitherT.pure[ContextState, Error](()))
              caseExprType <- pureTypeOf(v._2)
            } yield typeShift(-patternTypeInfo._2, caseExprType)
          )
        }
        // Finally, check if all the case expressions have the same type.
        caseExprType <- caseExprTypes
          .traverse(caseExprType =>
            isTypeEqualWithTypeError(
              caseExprType,
              caseExprTypes.head,
              MatchCasesTypeError(
                info,
                caseExprType,
                caseExprTypes.head
              )
            )
          )
          .map(_.head)
      } yield caseExprType
    case TermTag(info, tag, t1, ty1) =>
      for {
        tyS <- EitherT.liftF(simplifyType(ty1))
        tySVar <- tyS match {
          case TypeRec(_, _, v @ TypeVariant(_)) => v.pure[StateEither]
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
    case TermTrue(_)        => (TypeBool: Type).pure[StateEither]
    case TermFalse(_)       => (TypeBool: Type).pure[StateEither]
    case TermInt(_, _)      => (TypeInt: Type).pure[StateEither]
    case TermFloat(_, _)    => (TypeFloat: Type).pure[StateEither]
    case TermString(_, _)   => (TypeString: Type).pure[StateEither]
    case TermUnit(_)        => (TypeUnit: Type).pure[StateEither]
    case TermBuiltin(ty) => ty.pure[StateEither]
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
      simpleExprType: Type,
      exprType: Type
  ): StateEither[(Option[Type], Int)] =
    p match {
      case t: Term        => typeOf(t).map(v => (Some(v), 0))
      case PatternDefault(_) => EitherT.pure((None, 0))
      case PatternNode(info, node, vars) =>
        simpleExprType match {
          case TypeVariant(fields) =>
            for {
              ty <- fields
                .find(_._1 == node)
                .map { case (_, v) => v.pure[StateEither] }
                .getOrElse(
                  TypeError.format(
                    MatchVariantPatternMismatchTypeError(
                      info,
                      exprType,
                      node
                    )
                  )
                )
              numOfBindVars <- ty match {
                case tyR @ TypeRecord(_) =>
                  typeOfPattern(p, tyR, exprType).map(_._2)
                case _ => 0.pure[StateEither]
              }
            } yield (Some(simpleExprType), numOfBindVars)
          case TypeRecord(fields) =>
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
                      exprType,
                      node
                    )
                  )
                )
              _ <- Context.getType(info, idx)
              _ <- bindFieldsToVars(info, fields, vars, simpleExprType)
            } yield (Some(simpleExprType), fields.length)
          case _ =>
            TypeError.format(
              MatchExprNotDataTypeError(info, simpleExprType, node)
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
          fields.zipWithIndex.traverse { case ((f, i)) =>
            Context.addBinding(f._1, VarBind(typeShift(i, f._2)))
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
        k <- k1 match {
          case KindArrow(k11, k12) =>
            typeErrorEitherCond(
              k11 == k2,
              k12,
              KindParameterMismatchTypeError(
                UnknownInfo,
                tyT2,
                k2,
                k11
              )
            )
          case _ => TypeError.format(NoTypArgumentsTypeError(UnknownInfo, tyT1))
        }
      } yield k
    case TypeRec(tyX, k1, tyT2) =>
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
        InvalidTypeArgumentTypeError(UnknownInfo, ty)
      )
    }

  def getKind(idx: Int): StateEither[Kind] =
    Context
      .getBinding(idx)
      .flatMap(_ match {
        case TypeVarBind(k)          => EitherT.rightT(k)
        case TypeAbbBind(_, Some(k)) => EitherT.rightT(k)
        case TypeAbbBind(_, None) =>
          TypeError.format(NoKindForTypeError(UnknownInfo, idx))
        case _ => TypeError.format(BindingNotFoundTypeError(UnknownInfo))
      })
}
