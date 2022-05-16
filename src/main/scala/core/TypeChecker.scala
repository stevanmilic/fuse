package core

import cats.data.EitherT
import cats.data.OptionT
import cats.data.State
import cats.data.StateT
import cats.implicits.*
import core.Bindings.*
import core.Context.*
import core.Shifting.*
import core.Terms.*
import core.Types.*
import parser.FuseParser.*
import parser.Info.*
import parser.Info.ShowInfo.*

import scala.annotation.tailrec
import scala.util.*

object TypeChecker {
  val MainFunction = "main"

  def run(binds: List[Bind]): Either[Error, List[Bind]] =
    checkBindings(binds).value.runEmptyA.value

  def checkBindings(binds: List[Bind]): StateEither[List[Bind]] =
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
      pureInfer(term).map(ty => TermAbbBind(term, Some(ty)))
    case _ => EitherT.rightT(b)
  }

  def pureInfer(t: Term): StateEither[Type] = for {
    m <- EitherT.liftF(addMark("p"))
    iT <- infer(t)
    aT <- apply(iT)
    _ <- EitherT.liftF(peel(m))
  } yield aT

  /** Infers a type for `exp` with input context `ctx`.
    * @return
    *   the inferred type and the output context.
    */
  def infer(exp: Term): StateEither[Type] = exp match {
    case TermAscribe(info, t1, typeToAscribe) =>
      for {
        _ <- checkKindStar(typeToAscribe)
        tyT1 <- pureInfer(t1)
        t <- isSubtypeWithTypeError(
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
        tyT2 <- pureInfer(expr)
      } yield TypeArrow(info, variableType, typeShift(-1, tyT2))
    case TermClosure(info, variable, None, expr) =>
      for {
        eA <- EitherT.liftF(addEVar("a", info, TypeEFreeBind))
        eC <- EitherT.liftF(addEVar("c", info, TypeEFreeBind))
        b <- EitherT.liftF(addBinding(variable, VarBind(eA)))
        result <- check(expr, eC)
        _ <- EitherT.liftF(peel(b))
        eAS <- apply(eA).flatMap(t => EitherT.liftF(typeShiftOnContextDiff(t)))
        eCS <- apply(eC)
      } yield TypeArrow(info, eAS, typeShift(-1, eCS))
    case TermAbs(info, variable, variableType, expr, returnType) =>
      for {
        _ <- checkKindStar(variableType)
        _ <- EitherT.liftF(Context.addBinding(variable, VarBind(variableType)))
        exprType <- pureInfer(expr)
        _ <- returnType match {
          case Some(ty) =>
            isSubtypeWithTypeError(
              ty,
              exprType,
              WrongReturnTypeError(ty.info, exprType, ty)
            )
          case _ => ().pure[StateEither]
        }
      } yield TypeArrow(info, variableType, typeShift(-1, exprType))
    case TermApp(info, fun, arg) =>
      for {
        funType <- infer(fun)
        simplifiedFun <- EitherT.liftF(simplifyType(funType))
        ty <- inferApp(simplifiedFun, arg)
      } yield ty
    case TermProj(info, ty, label) =>
      for {
        tyT1 <- pureInfer(ty)
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
        tyT1 <- pureInfer(term)
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
                  {
                    val methodID = Desugar.toMethodId(method, typeName)
                    nameToIndex(ctx, methodID).orElse(
                      nameToIndex(ctx, Desugar.toRecAbsId(methodID))
                    )
                  }
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
          case (TypeAll(_, _, _, tpe), _) =>
            TypeError.format(MissingTypeAnnotation(info, typeShift(-1, tyT1S)))
          case _ => TypeError.format(NoMethodsOnTypeError(info, tyT1S))
        }
      } yield fieldType
    case TermFix(info, t1) =>
      for {
        tyT1 <- infer(t1)
        tyT1S <- EitherT.liftF(simplifyType(tyT1))
        bodyType <- tyT1S match {
          case TypeArrow(_, tyT11, tyT12) => tyT12.pure[StateEither]
          case _ =>
            TypeError.format(InvalidFunctionTypeError(info, tyT1S))
        }
      } yield bodyType
    case TermRecord(info, fields) =>
      fields
        .traverse { case (v, term) =>
          pureInfer(term).map((v, _))
        }
        .map(TypeRecord(info, _))
    case TermFold(info, tyS) =>
      EitherT
        .liftF(simplifyType(tyS))
        .flatMap(_ match {
          case TypeRec(_, _, _, tyT) =>
            (TypeArrow(info, typeSubstituteTop(tyS, tyT), tyS): Type).pure
          case _ =>
            TypeError.format(InvalidFoldForRecursiveTypeError(info, tyS))
        })
    case TermLet(_, variable, t1, t2) =>
      for {
        tyT1 <- infer(t1)
        _ <- EitherT.liftF(addBinding(variable, VarBind(tyT1)))
        tyT2 <- pureInfer(t2)
      } yield typeShift(-1, tyT2)
    case TermTAbs(info, v, t) =>
      for {
        _ <- EitherT.liftF(addBinding(v, TypeVarBind(KindStar)))
        ty <- pureInfer(t).map(TypeAll(info, v, KindStar, _))
      } yield ty
    case TermTApp(info, expr, ty2) =>
      for {
        k2 <- kindOf(ty2)
        ty1 <- pureInfer(expr)
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
        exprType <- pureInfer(exprTerm)
        caseType <- cases.foldRight((None: Option[Type]).pure[StateEither]) {
          case ((pattern, term), acc) =>
            for {
              a <- acc
              mark <- EitherT.liftF(addMark("m"))
              appExprType <- exprType match {
                case TypeEVar(_, _) => apply(exprType)(shift = false)
                case _              => exprType.pure[StateEither]
              }
              (patternType, variables) <- inferPattern(pattern, appExprType)
              caseExprType <- pureInfer(term)
              _ <- a match {
                case None    => true.pure[StateEither]
                case Some(t) => subtype(caseExprType, t)
              }
              _ <- patternType
                .map(subtype(appExprType, _))
                .getOrElse(EitherT.pure[ContextState, Error](()))
              cT <- caseExprType match {
                case TypeEVar(_, _) => apply(caseExprType)(shift = false)
                case _ =>
                  typeShift(-variables.length, caseExprType)
                    .pure[StateEither]
              }
              _ <- EitherT.liftF(peel(mark))
            } yield Some(cT)
        }
      } yield caseType.getOrElse(TypeUnit(info))
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
        tyTi <- pureInfer(t1)
        _ <- isTypeEqualWithTypeError(
          tyTi,
          tyTiExpected,
          TagFieldMismatchTypeError(info, tyTi, tyTiExpected)
        )
      } yield typeSubstituteTop(ty1, tySVar)
    case TermTrue(info)      => TypeBool(info).pure
    case TermFalse(info)     => TypeBool(info).pure
    case TermInt(info, _)    => TypeInt(info).pure
    case TermFloat(info, _)  => TypeFloat(info).pure
    case TermString(info, _) => TypeString(info).pure
    case TermUnit(info)      => TypeUnit(info).pure
    case TermBuiltin(ty)     => ty.pure
  }

  /** Infers the type of an application of a function of type `fun` to `exp`.
    * See Figure 11.
    * @return
    *   the inferred type and the output context.
    */
  def inferApp(funType: Type, exp: Term): StateEither[Type] = funType match {
    case TypeAll(info, _, _, tpe) =>
      for {
        eV <- EitherT.liftF(addEVar("a", info, TypeEFreeBind))
        reduced = typeSubstituteTop(eV, tpe)
        ty <- inferApp(reduced, exp)
      } yield ty
    case eA: TypeEVar =>
      for {
        (a1, a2, _) <- insertEArrow(eA)
        _ <- check(exp, a1)
      } yield a2
    case TypeArrow(_, argT, restT) => check(exp, argT).map(_ => restT)
    case _ =>
      TypeError.format(
        VariableNotFunctionTypeError(
          exp.info,
          funType
        )
      )
  }

  def typeErrorEitherCond[T](
      cond: Boolean,
      value: T,
      error: TypeError
  ): StateEither[T] =
    cond match {
      case true  => value.pure[StateEither]
      case false => TypeError.format(error)
    }

  def inferPattern(
      p: Pattern,
      matchExprType: Type
  ): StateEither[(Option[Type], List[String])] =
    p match {
      case t: Term                  => infer(t).map(v => (Some(v), List()))
      case PatternDefault(_)        => EitherT.pure((None, List()))
      case n @ PatternNode(_, _, _) => inferNodePattern(n, matchExprType)
    }

  def inferNodePattern(
      p: PatternNode,
      matchExprType: Type
  ): StateEither[(Option[Type], List[String])] = for {
    unfoldedExprType <- EitherT.liftF(unfoldType(matchExprType))
    v <- unfoldedExprType match {
      case TypeVariant(_, fields) =>
        for {
          ty <- fields
            .find(_._1 == p.tag)
            .map { case (_, v) => v.pure[StateEither] }
            .getOrElse(
              TypeError.format(
                MatchVariantPatternMismatchTypeError(
                  p.info,
                  matchExprType,
                  p.tag
                )
              )
            )
          variables <- ty match {
            case tyR @ TypeRecord(_, _) =>
              inferPattern(p, tyR).map(_._2)
            case _ => List().pure[StateEither]
          }
        } yield (Some(matchExprType), variables)
      case TypeRecord(_, fields) =>
        for {
          optionIdx <- EitherT.liftF(
            State.inspect((ctx: Context) =>
              Context
                .nameToIndex(ctx, p.tag)
            )
          )
          idx <- optionIdx
            .map(_.pure[StateEither])
            .getOrElse(
              TypeError.format(
                MatchRecordPatternMismatchTypeError(
                  p.info,
                  matchExprType,
                  p.tag
                )
              )
            )
          _ <- Context.getType(p.info, idx)
          variables <- bindFieldsToVars(
            p.info,
            fields,
            p.vars,
            matchExprType
          )
        } yield (Some(matchExprType), variables)
      case TypeEVar(info, _) => inferNodePatternWithEVar(info, p)
      case _ =>
        TypeError.format(
          MatchExprNotDataTypeError(p.info, matchExprType, p.tag)
        )
    }
  } yield v

  def inferNodePatternWithEVar(
      info: Info,
      p: PatternNode
  ): StateEither[(Option[Type], List[String])] = for {
    optionType <- EitherT.liftF(getAlgebraicDataTypeVar(info, p.tag))
    v <- optionType match {
      case Some(ty) =>
        for {
          tyS <- EitherT.liftF(simplifyType(ty))
          eVars <- EitherT.liftF(extractEVarsFromTypeAbs(tyS))
          tyA = eVars.foldLeft(ty: Type)((acc, eA) => TypeApp(eA.info, acc, eA))
          (tP, vars) <- inferPattern(p, tyA)
        } yield (tP, vars)
      case _ => TypeError.format(MatchNodePatternNotFound(p.info, p))
    }
  } yield v

  def extractEVarsFromTypeAbs(ty: Type): ContextState[List[TypeEVar]] = {
    def iter(t: Type, acc: List[TypeEVar]) = t match {
      case TypeAbs(info, i, tyB) =>
        for {
          eA <- addEVar(i, info, TypeEFreeBind)
          a <- extractEVarsFromTypeAbs(tyB)
        } yield eA :: a
      case _ => acc.pure[ContextState]
    }
    iter(ty, List())
  }

  def bindFieldsToVars(
      info: Info,
      fields: List[(String, Type)],
      vars: List[String],
      exprType: Type
  ): StateEither[List[String]] =
    fields.length == vars.length match {
      case true =>
        val t = fields.zipWithIndex.zip(vars).traverse { case ((f, i), v) =>
          isRecursiveType(f._2).map(isRecType => {
            val variableName = isRecType match {
              case true  => s"$v'"
              case false => v
            }
            (variableName, f._2, i)
          })
        }
        EitherT.right(
          t.flatMap(v =>
            v.traverse { case (v, ty, idx) =>
              Context.addBinding(v, VarBind(typeShift(idx, ty)))
            }
          )
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

  def unfoldType(tyS: Type): ContextState[Type] =
    simplifyType(tyS).map(_ match {
      case TypeRec(_, _, _, tyT) =>
        typeSubstituteTop(findRootType(tyS), tyT)
      case ty => ty
    })

  def simplifyType(ty: Type): ContextState[Type] =
    for {
      tyA <- apply(ty).getOrElse(ty)
      tyT <- tyA match {
        case TypeApp(info, tyT1, tyT2) =>
          simplifyType(tyT1).map(TypeApp(info, _, tyT2))
        // NOTE: Here we return the original type, not the we tried to apply.
        // Since we wanna extract type application only for applied types.
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

  def isSubtypeWithTypeError(
      ty1: Type,
      ty2: Type,
      error: TypeError
  ): StateEither[Type] = for {
    isEqual <- subtype(ty1, ty2)
    t <- typeErrorEitherCond(
      isEqual,
      ty1,
      error
    )
  } yield t

  def isRecursiveType(ty: Type): ContextState[Boolean] =
    simplifyType(ty)
      .map(_ match {
        case _: TypeRec => true
        case v          => false
      })

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

  /** Checks (solves exist vars) that `exp` has type `t` with input context
    * `ctx`.
    */
  def check(exp: Term, t: Type): StateEither[Unit] = (exp, t) match {
    // 1I :: ((), 1)
    case (TermUnit(_), TypeUnit(_))        => ().pure
    case (TermFloat(_, _), TypeFloat(_))   => ().pure
    case (TermString(_, _), TypeString(_)) => ().pure
    case (TermInt(_, _), TypeInt(_))       => ().pure
    case (TermTrue(_), TypeBool(_))        => ().pure
    case (TermFalse(_), TypeBool(_))       => ().pure
    // ->I :: (λx.e, A→B)
    case (TermClosure(_, arg, None, exp), TypeArrow(_, argT, expT)) =>
      for {
        b <- EitherT.liftF(addBinding(arg, VarBind(argT)))
        _ <- check(exp, expT) // Γ,α ⊢ e ⇐ A ⊣ ∆,α,Θ
        _ <- EitherT.liftF(peel(b))
      } yield ()
    // ∀I :: (e, ∀α.A)
    case (exp, TypeAll(_, uA, k, tpe)) =>
      for {
        b <- EitherT.liftF(addBinding(uA, TypeVarBind(k)))
        _ <- check(exp, tpe)
        _ <- EitherT.liftF(peel(b))
      } yield ()
    // Sub :: (e, B)
    case (exp, tpe) =>
      for {
        expType <- infer(exp) // Γ ⊢ e ⇒ A ⊣ Θ
        redExpType <- apply(expType)
        reducedType <- apply(tpe)
        r <- subtype(redExpType, reducedType)
        _ <- r match {
          case true => ().pure[StateEither]
          case false =>
            TypeError.format(
              InvalidSubtypeTypeError(exp.info, redExpType, reducedType)
            )
        }
      } yield ()
  }

  /** Applies `ctx` to `tpe` (substituting existential vars for their
    * solutions).
    */
  def apply(t: Type)(implicit shift: Boolean = true): StateEither[Type] =
    t match {
      case ev: TypeEVar =>
        EitherT
          .liftF(Context.solution(ev, shift))
          .flatMap(ty =>
            ty.map(apply(_)).getOrElse((ev: Type).pure[StateEither])
          )
      case TypeArrow(info, a, b) =>
        for {
          t1 <- apply(a)
          t2 <- apply(b)
        } yield TypeArrow(info, t1, t2)
      case TypeApp(info, a, b) =>
        for {
          t1 <- apply(a)
          t2 <- apply(b)
        } yield TypeApp(info, t1, t2)
      case TypeAll(info, i, k, tpe) => apply(tpe).map(TypeAll(info, i, k, _))
      case _                        => t.pure[StateEither]
    }

  /** Derives a subtyping relationship `tpeA <: tpeB` with input context `ctx`.
    * See Figure 9.
    * @return
    *   the output context.
    */
  def subtype(tpeA: Type, tpeB: Type): StateEither[Boolean] =
    (tpeA, tpeB) match {
      // <:Exvar :: Γ[â] ⊢ â <: â ⊣ Γ[â]
      case (eA @ TypeEVar(info, n1), TypeEVar(_, n2)) if (n1 == n2) =>
        EitherT
          .liftF(containsFreeEVar(eA))
          .flatMap(_ match {
            case true => true.pure[StateEither]
            case false =>
              TypeError.format(UnboundExistentialVariableTypeError(info, eA))
          })
      // <:→ :: Γ ⊢ A1→A2 <: B1→B2 ⊣ ∆
      case (TypeArrow(_, a1, a2), TypeArrow(_, b1, b2)) =>
        for {
          s1 <- subtype(b1, a1)
          redA2 <- apply(a2)
          redB2 <- apply(b2)
          s2 <- subtype(redA2, redB2)
        } yield s1 && s2
      case (TypeApp(_, a1, a2), TypeApp(_, b1, b2)) =>
        for {
          s1 <- subtype(b1, a1)
          redA2 <- apply(a2)
          redB2 <- apply(b2)
          s2 <- subtype(redA2, redB2)
        } yield s1 && s2
      // <:∀L :: Γ ⊢ ∀α.A <: B ⊣ ∆
      case (TypeAll(info, _, _, a), b) =>
        for {
          eAMark <- EitherT.liftF(Context.addMark("a"))
          eA <- EitherT.liftF(Context.addEVar("a", info, TypeEFreeBind))
          r <- subtype(typeSubstituteTop(eA, a), b)
          _ <- EitherT.liftF(peel(eAMark))
        } yield r
      // <:∀R :: Γ ⊢ A <: ∀α.B ⊣ ∆
      case (a, TypeAll(_, uB, k, b)) =>
        for {
          n <- EitherT.liftF(Context.addBinding(uB, TypeVarBind(k)))
          r <- subtype(a, b)
          _ <- EitherT.liftF(peel(n))
        } yield r
      // <:InstantiateL :: Γ[â] ⊢ â <: A ⊣ ∆
      case (eA: TypeEVar, a) =>
        for {
          isFree <- EitherT.liftF(Context.containsFreeEVar(eA))
          hasFree = a.containsEVar(eA)
          r <- isFree && !hasFree match {
            case false => false.pure[StateEither]
            case true  => instantiateL(eA, a).map(_ => true)
          }
        } yield r
      // <:InstantiateR :: Γ[â] ⊢ A <: â ⊣ ∆
      case (a, eA: TypeEVar) =>
        for {
          isFree <- EitherT.liftF(Context.containsFreeEVar(eA))
          hasFree = a.containsEVar(eA)
          r <- isFree && !hasFree match {
            case false => false.pure[StateEither]
            case true  => instantiateR(a, eA).map(_ => true)
          }
        } yield r
      case _ =>
        for {
          r <- EitherT.liftF(isTypeEqual(tpeA, tpeB))
          _ <- r match {
            case true  => true.pure[StateEither]
            case false => false.pure[StateEither]
          }
        } yield r
    }

  /** Instantiates `eA` such that `eA <: a` in `ctx`. See Figure 10.
    * @return
    *   the output context.
    */
  def instantiateL(eA: TypeEVar, a: Type): StateEither[Unit] = {
    def iter(
        isSimple: Boolean,
        containsEVarWithPeel: Boolean,
        containsEVar: Boolean
    ) = (a, isSimple, containsEVarWithPeel, containsEVar) match {
      case (a, true, _, _) /* Γ ⊢ τ */ => solve(eA, a) // Γ,â=τ,Γ′
      case (eC: TypeEVar, _, true, _)  => solve(eC, eA)
      case (TypeApp(_, a1, a2), _, _, true) =>
        for {
          (eA1, eA2, _) <- insertEApp(eA)
          _ <- instantiateR(a1, eA1)
          redA2 <- apply(a2)
          _ <- instantiateL(eA2, redA2)
        } yield ()
      // InstLArr :: Γ[â] ⊢ â :=< A1 → A2 ⊣ ∆
      case (TypeArrow(info, a1, a2), _, _, true) =>
        for {
          (eA1, eA2, _) <- insertEArrow(eA)
          _ <- instantiateR(a1, eA1)
          redA2 <- apply(a2)
          _ <- instantiateL(eA2, redA2)
        } yield ()
      // InstLAllR :: Γ[â] ⊢ â :=< ∀β.B ⊣ ∆
      case (TypeAll(_, uB, k, b), _, _, true) =>
        for {
          v <- EitherT.liftF(Context.addBinding(uB, TypeVarBind(k)))
          // Γ[â],β ⊢ â :=< B ⊣ ∆,β,∆′
          r <- instantiateL(eA, b)
          _ <- EitherT.liftF(peel(v))
        } yield r
      case _ => TypeError.format(FailedToInstantiateTypeError(eA.info, eA, a))
    }
    for {
      b1 <- isMonoAndWellFormed(a, eA)
      b2 <- EitherT.liftF(containsEVarWithPeel(eA, a))
      b3 <- EitherT.liftF(containsFreeEVar(eA))
      v <- iter(b1, true, b3)
    } yield v
  }

  /** Instantiates `eA` such that `a <: eA` in `ctx`. See Figure 10.
    * @return
    *   the output context.
    */
  def instantiateR(a: Type, eA: TypeEVar): StateEither[Unit] = {
    def iter(
        isSimple: Boolean,
        containsEVarWithPeel: Boolean,
        containsEVar: Boolean
    ) = (a, isSimple, containsEVarWithPeel, containsEVar) match {
      case (a, true, _, _) /* Γ ⊢ τ */ => solve(eA, a) // Γ,â=τ,Γ′
      case (eC: TypeEVar, _, true, _)  => solve(eC, eA)
      case (TypeApp(_, a1, a2), _, _, true) =>
        for {
          (eA1, eA2, _) <- insertEApp(eA)
          _ <- instantiateL(eA1, a1)
          redA2 <- apply(a2)
          _ <- instantiateR(redA2, eA2)
        } yield ()
      // InstRArr :: Γ[â] ⊢ A1 → A2 :=< â ⊣ ∆
      case (TypeArrow(info, a1, a2), _, _, true) =>
        for {
          (eA1, eA2, _) <- insertEArrow(eA)
          _ <- instantiateL(eA1, a1)
          redA2 <- apply(a2)
          _ <- instantiateR(redA2, eA2)
        } yield ()
      // InstRAllL :: Γ[â],▶ĉ,ĉ ⊢ [ĉ/β]B :=< â ⊣ ∆,▶ĉ,∆′
      case (TypeAll(info, _, _, b), _, _, true) =>
        for {
          eCMark <- EitherT.liftF(Context.addMark("c"))
          eC <- EitherT.liftF(Context.addEVar("c", info, TypeEFreeBind))
          // Γic ⊢ [ĉ/β]B :=< â ⊣ ∆,▶ĉ,∆′
          r <- instantiateR(typeSubstituteTop(eC, b), eA)
          _ <- EitherT.liftF(peel(eCMark)) // ∆
        } yield r
      case _ => TypeError.format(FailedToInstantiateTypeError(eA.info, a, eA))
    }
    for {
      b1 <- isMonoAndWellFormed(a, eA)
      b2 <- EitherT.liftF(containsEVarWithPeel(eA, a))
      b3 <- EitherT.liftF(containsFreeEVar(eA))
      v <- iter(b1, b2, b3)
    } yield v
  }

  def isMonoAndWellFormed(a: Type, eA: TypeEVar): StateEither[Boolean] =
    a.isMono match {
      case true  => EitherT.liftF(isWellFormedWithPeel(a, eA))
      case false => false.pure
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
}
