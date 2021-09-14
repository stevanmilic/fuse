package core

import cats.data.EitherT
import cats.data.State
import cats.data.StateT
import cats.implicits._
import core.Context._
import parser.Expressions._
import parser.FuseParser._
import parser.Identifiers.UnknownInfo
import parser.Identifiers._
import parser.Types._

object Desugar {

  def process(decls: List[FDecl]): StateEither[List[Bind]] =
    decls.traverse(bind(_)).map(_.flatten)

  // # Bind # region_start

  def bind(d: FDecl): StateEither[List[Bind]] = d match {
    case FVariantTypeDecl(FIdentifier(i), typ, values) =>
      val variant = toTypeVariant(values)
      val rec = withRecType(i, typ, variant)
      for {
        variantType <- Context.runE(withTypeAbs(typ, rec))
        typeBind <- bindTypeAbb(i, variantType)
        constructorBinds <- values.toList.traverse(v =>
          for {
            constructor <- Context.runE(buildVariantConstructor(i, typ, v))
            constructorBind <- bindTermAbb(v.v.value, constructor)
          } yield constructorBind
        )
      } yield typeBind :: constructorBinds
    case FRecordTypeDecl(FIdentifier(i), typ, fields) =>
      val record = toTypeRecord(fields.map(_.p))
      val rec = withRecType(i, typ, record)
      for {
        recordType <- Context.runE(withTypeAbs(typ, rec))
        typeBind <- bindTypeAbb(i, recordType)
        constructor <- Context.runE(
          buildRecordConstructor(i, typ, Left(fields.map(_.p)))
        )
        constructorBind <- bindTermAbb(toRecordConstructorId(i), constructor)
      } yield typeBind :: List(constructorBind)
    case FTupleTypeDecl(FIdentifier(i), typ, types) =>
      val tuple = toTupleTypeRecord(types)
      val rec = withRecType(i, typ, tuple)
      for {
        recordType <- Context.runE(withTypeAbs(typ, rec))
        typeBind <- bindTypeAbb(i, recordType)
        constructor <- Context.runE(
          buildRecordConstructor(i, typ, Right(types))
        )
        constructorBind <- bindTermAbb(toRecordConstructorId(i), constructor)
      } yield typeBind :: List(constructorBind)
    case FTypeAlias(FIdentifier(i), typ, t) =>
      val alias = toType(t)
      for {
        alias <- Context.runE(withTypeAbs(typ, alias))
        typeBind <- bindTypeAbb(i, alias)
      } yield List(typeBind)
    case FFuncDecl(sig @ FFuncSig(FIdentifier(i), tp, _, _), exprs) => {
      for {
        func <- Context.runE(buildFunc(tp, sig, exprs))
        funcBind <- bindTermAbb(i, func)
      } yield List(funcBind)
    }
    case FTypeFuncDecls(typeIdentifier, typeParams, functions) =>
      val param = FParam(
        FIdentifier("this"),
        FSimpleType(
          typeIdentifier,
          Some(
            typeParams
              .getOrElse(Seq())
              .map(tp => FSimpleType(FIdentifier(tp.i.value)))
          )
        )
      )
      val modifySignature = (sig: FFuncSig) =>
        FFuncSig(
          FIdentifier(toMethodId(sig.i.value, typeIdentifier.value)),
          typeParams.fold(sig.tp)(p => Some(sig.tp.fold(p)(p ++ _))),
          Some(sig.p.fold(Seq(param))(param +: _)),
          sig.r
        )
      functions.toList
        .traverse(f => bind(FFuncDecl(modifySignature(f.sig), f.exprs)))
        .map(_.flatten)

    case _ =>
      DesugarError.format(DeclarationNotSupportedDesugarError(UnknownInfo))
  }

  def bindTypeAbb(i: String, t: Type): StateEither[Bind] =
    EitherT.liftF(Context.addName(i).map(Bind(_, TypeAbbBind(t))))

  def bindTermAbb(i: String, t: Term): StateEither[Bind] =
    EitherT.liftF(Context.addName(i).map(Bind(_, TermAbbBind(t))))

  // # Bind # region_end

  // # Term # region_start

  def buildFunc(
      tp: FTypeParamClause,
      sig: FFuncSig,
      exprs: Seq[FExpr]
  ): StateEither[Term] = for {
    t <- EitherT.liftF(withTermTypeAbs(tp))
    abs <- withFuncAbs(sig, toTermExpr(exprs.toList))
  } yield t(abs)

  def withFuncAbs(
      sig: FFuncSig,
      body: StateEither[Term]
  ): StateEither[Term] = sig.p match {
    case Some(params) =>
      val func = params.zipWithIndex
        .foldRight(body) { case ((p, index), acc) =>
          for {
            typ <- toType(p.t)
            variable <- EitherT.liftF(Context.addName(p.i.value))
            term <- acc
            retType <- toType(sig.r)
            // NOTE: The return type specified by the function signature
            // should be added to the last nested abstraction, since that one
            // is returning the value for the function.
            retVal =
              if (index == params.length - 1) Some(retType) else None
          } yield TermAbs(variable, typ, term, retVal)
        }
      withFixCombinator(sig.i.value, params.map(_.t).toList, sig.r, func)
    case None =>
      for {
        _ <- EitherT.liftF(Context.addName(WildcardName))
        typ <- toType(sig.r)
        term <- body
      } yield TermAbs(WildcardName, TypeUnit, term, Some(typ))
  }

  def toTermExpr(
      exprs: List[FExpr],
      letVariable: Option[(String, Option[FType])] = None
  ): StateEither[Term] = exprs match {
    case Nil => EitherT.rightT(TermUnit)
    case l @ FLetExpr(i, t, e) :: lexprs =>
      for {
        lt <- withTermLet(i.value, t, e.toList)
        le <- toTermExpr(lexprs)
      } yield lt(le)
    case h :: Nil => toTerm(h, letVariable)
    case _        => DesugarError.format(ExpressionNotValidDesugarError(UnknownInfo))
  }

  def withTermLet(
      i: String,
      t: Option[FType],
      expr: List[FExpr]
  ): StateEither[Term => Term] =
    for {
      t1 <- toTermExpr(expr, Some((i, t)))
      v <- EitherT.liftF(Context.addName(i))
    } yield t2 => TermLet(v, t1, t2): Term

  def toTerm(
      e: FExpr,
      letVariable: Option[(String, Option[FType])] = None
  ): StateEither[Term] =
    e match {
      case FApp(e, typeArgs, args) =>
        for {
          exprTerm <- toTerm(e)
          typedTerm <- typeArgs
            .getOrElse(Seq())
            .toList
            .traverse(toType(_))
            .map(_.foldLeft(exprTerm)((term, ty) => TermTApp(term, ty)))
          computedTerm <- args.toList.flatten.flatten
            .traverse(toTerm(_))
            .map(
              _.foldLeft(typedTerm)((term, arg) => TermApp(term, arg))
            )
        } yield computedTerm
      case FMatch(e, cases) =>
        for {
          me <- toTerm(e)
          mc <- cases.toList.traverse(c => {
            val exprList = c.e.toList
            c.p.toList.traverse(p => Context.runE(toMatchCase(p, exprList)))
          })
        } yield TermMatch(me, mc.flatten)
      case FProj(expr, projections) => toTermProj(expr, projections)
      case FMethodApp(proj, typeArgs, args) =>
        for {
          termProj <- toTermProj(proj.e, proj.ids.tail)
          methodProj = TermMethodProj(termProj, proj.ids.head.value)
          typedTerm <- typeArgs
            .getOrElse(Seq())
            .toList
            .traverse(toType(_))
            .map(_.foldLeft(methodProj: Term)((term, ty) => TermTApp(term, ty)))
          withImplicitThis = TermApp(typedTerm, methodProj.t)
          computedTerm <- args.toList.flatten.flatten
            .traverse(toTerm(_))
            .map(
              _.foldLeft(withImplicitThis)((term, arg) => TermApp(term, arg))
            )
        } yield computedTerm
      case FAbs(bindings, Some(rType), expr) =>
        letVariable match {
          case Some((f, _)) =>
            val closure = withClosure(bindings.toList, toTermExpr(expr.toList))
            Context.runE(
              withFixCombinator(f, bindings.map(_.t.get).toList, rType, closure)
            )
          case _ =>
            withClosure(bindings.toList, toTermExpr(expr.toList))
        }
      case FAbs(bindings, _, expr) =>
        withClosure(bindings.toList, toTermExpr(expr.toList))
      case FMultiplication(i1, i2) =>
        toTermOperator("&multiply", i1, i2)
      // TODO: Add other operators.
      case FAddition(i1, i2) =>
        toTermOperator("&add", i1, i2)
      case FSubtraction(i1, i2) =>
        toTermOperator("&sub", i1, i2)
      case FEquality(i1, i2) =>
        toTermOperator("&eq", i1, i2)
      case FVar(i) =>
        EitherT
          .liftF(toTermVar(i))
          .flatMap(_ match {
            case Some(v) => v.pure[StateEither]
            case None =>
              DesugarError.format(VariableNotFoundDesugarError(UnknownInfo, i))
          })
      case FBool(true)  => EitherT.rightT(TermTrue)
      case FBool(false) => EitherT.rightT(TermFalse)
      case FInt(i)      => EitherT.rightT(TermInt(i))
      case FFloat(f)    => EitherT.rightT(TermFloat(f))
      case FString(s)   => EitherT.rightT(TermString(s))
      case _ =>
        DesugarError.format(ExpressionNotSupportedDesugarError(UnknownInfo))
    }

  def toTermProj(e: FInfixExpr, ids: Seq[FVar]): StateEither[Term] =
    toTerm(e).map(t =>
      ids.foldLeft(t)((terms, proj) => TermProj(terms, proj.value))
    )

  def withClosure(
      params: List[FBinding],
      body: StateEither[Term]
  ): StateEither[Term] =
    params.foldRight(body) { case (FBinding(i, Some(t)), acc) =>
      for {
        typ <- toType(t)
        v <- EitherT.liftF(Context.addName(i.value))
        term <- acc
      } yield TermClosure(v, Some(typ), term)
    }

  def withFixCombinator(
      name: String,
      params: List[FType],
      returnType: FType,
      func: StateEither[Term]
  ): StateEither[Term] = for {
    // The implicit param is added to represent the function we abstract,
    // in order to provide possibility for recursive call. Note that the
    // abstraction is wrapped in a fix combinator.
    ty <- toType(FFuncType(params, returnType))
    variable <- EitherT.liftF(Context.addName(toRecAbsId(name)))
    term <- func
    // NOTE: The abstraction is wrapped with a fix combinator to implement
    // recursion.
  } yield TermFix(TermAbs(variable, ty, term))

  def toTermOperator(
      func: String,
      e1: FExpr,
      e2: FExpr
  ): StateEither[Term] = for {
    optionFuncVar <- EitherT.liftF(toTermVar(func))
    funcVar <- optionFuncVar match {
      case Some(v) => v.pure[StateEither]
      case None =>
        DesugarError.format(
          FunctionOperatorNotFoundDesugarError(UnknownInfo, func)
        )
    }
    t1 <- toTerm(e1)
    t2 <- toTerm(e2)

  } yield TermApp(TermApp(funcVar, t1), t2)

  def toMatchCase(
      p: FPattern,
      e: List[FExpr]
  ): StateEither[(Pattern, Term)] = for {
    p <- toPattern(p)
    ce <- toTermExpr(e)
  } yield (p, ce)

  def toPattern(p: FPattern): StateEither[Pattern] = p match {
    case FIdentifierPattern(v, _) => EitherT.rightT(PatternNode(v))
    case FVariantOrRecordPattern(t, ps) =>
      for {
        np <- ps.toList.traverse(toPattern(_))
        vars <- np.traverse(_ match {
          case PatternNode(v, List()) =>
            EitherT.liftF[ContextState, Error, String](Context.addName(v))
          case PatternDefault => WildcardName.pure[StateEither]
          case _ =>
            DesugarError.format[String](
              NestedPatternNotSupportedDesugarError(UnknownInfo)
            )
        })
      } yield PatternNode(t.value, vars)
    case FWildCardPattern => EitherT.rightT(PatternDefault)
    case FBool(true)      => EitherT.rightT(TermTrue)
    case FBool(false)     => EitherT.rightT(TermFalse)
    case FInt(i)          => EitherT.rightT(TermInt(i))
    case FFloat(f)        => EitherT.rightT(TermFloat(f))
    case FString(s)       => EitherT.rightT(TermString(s))
    case _                => DesugarError.format(CaseNotSupportedDesugarError(UnknownInfo))
  }

  def toTermVar(i: String): ContextState[Option[Term]] =
    State { ctx =>
      Context
        .nameToIndex(ctx, toRecordConstructorId(i))
        .orElse(Context.nameToIndex(ctx, i))
        .orElse(Context.nameToIndex(ctx, toRecAbsId(i))) match {
        case Some(index) => (ctx, Some(TermVar(index, ctx.length)))
        case None        => (ctx, None)
      }
    }

  def toRecAbsId(i: String): String = s"^$i"

  // # Term # region_end

  // # Type # region_start

  def toTypeVariant(v: Seq[FVariantTypeValue]): StateEither[Type] =
    v.toList
      .traverse(_ match {
        case FVariantTypeValue(FIdentifier(ti), None) =>
          ((ti, TypeUnit: Type)).pure[StateEither]
        case FVariantTypeValue(FIdentifier(ti), Some(Right(ts))) =>
          toTupleTypeRecord(ts).map((ti, _))
        case FVariantTypeValue(FIdentifier(ti), Some(Left(p))) =>
          toTypeRecord(p).map((ti, _))
      })
      .map(TypeVariant(_))

  def toTypeRecord(p: FParams): StateEither[Type] =
    p.toList.traverse(v => toType(v.t).map((v.i.value, _))).map(TypeRecord(_))

  def toTupleTypeRecord(ts: FTypes): StateEither[Type] =
    ts.toList.zipWithIndex
      .traverse(v => toType(v._1).map(((v._2 + 1).toString, _)))
      .map(TypeRecord(_))

  def withTypeAbs(
      tp: FTypeParamClause,
      typ: StateEither[Type]
  ): StateEither[Type] = tp match {
    case Some(params) =>
      val ids = params.map(_.i.value).toList
      for {
        _ <- EitherT.liftF(ids.traverse(Context.addName(_)))
        ty <- typ
      } yield ids.foldRight(ty)((p, acc) => TypeAbs(p, acc))
    case None => typ
  }

  def withRecType(
      i: String,
      tp: FTypeParamClause,
      typ: StateEither[Type]
  ): StateEither[Type] = for {
    ri <- EitherT.liftF(Context.addName(toRecId(i)))
    knd = List
      .fill(tp.map(_.length).getOrElse(0) + 1)(KindStar: Kind)
      .reduceRight(KindArrow(_, _))
    t <- typ
  } yield TypeRec(ri, knd, t)

  // # Type # region_end

  // # Constructors # region_start

  def buildVariantConstructor(
      variantName: String,
      tp: FTypeParamClause,
      v: FVariantTypeValue
  ): StateEither[Term] = v match {
    case FVariantTypeValue(FIdentifier(i), None) =>
      EitherT.liftF(for {
        g <- withTermTypeAbs(tp)
        tag = toTermTag(variantName, tp, i, State.pure(TermUnit))
        fold <- withFold(variantName, tp, tag)
      } yield g(fold))
    case FVariantTypeValue(FIdentifier(i), Some(fields)) =>
      for {
        g <- EitherT.liftF(withTermTypeAbs(tp))
        values = toRecordValues(fields)
        r = toTermRecord(values)
        tag = toTermTag(variantName, tp, i, r)
        fold = withFold(variantName, tp, tag)
        abs <- withRecordAbs(values, fold)
      } yield g(abs)
  }

  def buildRecordConstructor(
      recordName: String,
      tp: FTypeParamClause,
      fields: Either[FParams, FTypes]
  ): StateEither[Term] = for {
    g <- EitherT.liftF(withTermTypeAbs(tp))
    values = toRecordValues(fields)
    r = toTermRecord(values)
    fold = withFold(recordName, tp, r)
    abs <- withRecordAbs(values, fold)
  } yield g(abs)

  def toTermRecord(values: List[(String, FType)]): ContextState[Term] =
    values
      .traverse { case (n, t) =>
        toTermVar(withTupleParamId(n)).map(term => (n, term.get))
      }
      .map(TermRecord(_))

  def toTermTag(
      name: String,
      tp: FTypeParamClause,
      tag: String,
      body: ContextState[Term]
  ): ContextState[Term] = for {
    term <- body
    typ <- withTypeApp(name, tp)
  } yield TermTag(tag, term, typ)

  def toRecordValues(fields: Either[FParams, FTypes]) =
    fields match {
      case Left(params) => params.toList.map(f => (f.i.value, f.t))
      case Right(types) =>
        types.toList.zipWithIndex.map(v => ((v._2 + 1).toString, v._1))
    }

  def withTermTypeAbs(typ: FTypeParamClause): ContextState[Term => Term] =
    typ match {
      case Some(p) =>
        val ids = p.map(_.i.value).toList
        for {
          _ <- ids.traverse(Context.addName(_))
        } yield t => ids.foldRight(t)((i, acc) => TermTAbs(i, acc))
      case None => State.pure(identity)
    }

  def withRecordAbs(
      values: List[(String, FType)],
      body: ContextState[Term]
  ): StateEither[Term] =
    values
      .map { case (i, t) => (withTupleParamId(i), t) }
      .foldRight(EitherT.liftF[ContextState, Error, Term](body))((p, acc) =>
        for {
          typ <- toType(p._2)
          v <- EitherT.liftF(Context.addName(p._1))
          term <- acc
        } yield TermAbs(v, typ, term)
      )

  def withFold(
      name: String,
      tp: FTypeParamClause,
      body: ContextState[Term]
  ): ContextState[Term] =
    for {
      typ <- withTypeApp(name, tp)
      term <- body
    } yield TermApp(TermFold(typ), term)

  def withTypeApp(name: String, tp: FTypeParamClause): ContextState[Type] =
    tp match {
      case None => toTypeVarOrId(name)
      case Some(tys) =>
        tys
          .foldLeft(toTypeVarOrId(name))((acc, tp) =>
            for {
              typ <- toTypeVarOrId(tp.i.value)
              app <- acc
            } yield TypeApp(app, typ)
          )
    }

  // Prepends the identifier with "#" if it's an integer – depicting it's used
  // for the tuple records. If not, the identifier is returned unchanged.
  def withTupleParamId(i: String) = i.toIntOption match {
    case Some(v) => s"#$i"
    case None    => i
  }

  // The record constructor has a prefix "%" that should be searched during
  // type checking when the record type is found in the application.
  def toRecordConstructorId(i: String) = s"%$i"

  // The method type has a prefix "!" that should be searched during type
  // checking when the projection is found for a variable. There's also a "#"
  // separator that depicts the type name for the method.
  def toMethodId(methodName: String, typeName: String) =
    s"!${methodName}#${typeName}"

  // # Constructors # region_end

  def toType(t: FType): StateEither[Type] =
    t match {
      case FSimpleType(FIdentifier("i32"), None) =>
        EitherT.rightT(TypeInt: Type)
      case FSimpleType(FIdentifier("f32"), None)  => EitherT.rightT(TypeFloat)
      case FSimpleType(FIdentifier("bool"), None) => EitherT.rightT(TypeBool)
      case FSimpleType(FIdentifier("str"), None)  => EitherT.rightT(TypeString)
      case FSimpleType(FIdentifier(i), None)      => EitherT.liftF(toTypeVarOrId(i))
      case FSimpleType(FIdentifier(i), Some(tys)) =>
        tys.foldLeft(
          EitherT.liftF[ContextState, Error, Type](toTypeVarOrId(i))
        )((acc, typ) =>
          for {
            t <- toType(typ)
            app <- acc
          } yield (TypeApp(app, t): Type)
        )
      case FFuncType(ts, t1) =>
        for {
          i <- ts.toList.traverse(toType(_))
          o <- toType(t1)
          t = i match {
            case Nil => TypeArrow(TypeUnit, o)
            // Type abstractions (arrow) should be right associative.
            case _ => (i :+ o).reduceRight(TypeArrow(_, _))
          }
        } yield t
      case _ => DesugarError.format(TypeNotSupportedDesugarError(UnknownInfo))
    }

  def toTypeVarOrId(i: String): ContextState[Type] =
    State { ctx =>
      toCtxIndex(ctx, i) match {
        case Some(index) => (ctx, TypeVar(index, ctx.length))
        case None        => (ctx, TypeId(i))
      }
    }

  def toCtxIndex(ctx: Context, i: String): Option[Int] =
    Context.nameToIndex(ctx, i).orElse(Context.nameToIndex(ctx, toRecId(i)))

  def toRecId(i: String) = s"@$i"

}
