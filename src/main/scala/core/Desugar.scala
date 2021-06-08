package core

import cats.data.State
import cats.implicits._
import core.Context._
import parser.FuseExpressionParser._
import parser.FuseLexicalParser._
import parser.FuseParser._
import parser.FuseTypesParser._

object Desugar {

  def process(decls: List[FDecl]): ContextState[List[Bind]] =
    decls.traverse(bind(_)).map(_.flatten)

  // # Bind # region_start

  def bind(d: FDecl): ContextState[List[Bind]] = d match {
    case FVariantTypeDecl(FIdentifier(i), typ, values) =>
      val variant = toTypeVariant(values)
      val rec = withRecType(i, typ, variant)
      for {
        variantType <- Context.run(withTypeAbs(typ, rec))
        typeBind <- bindTypeAbb(i, variantType)
        constructorBinds <- values.toList.traverse(v =>
          for {
            constructor <- Context.run(buildVariantConstructor(i, typ, v))
            constructorBind <- bindTermAbb(v.v.value, constructor)
          } yield constructorBind
        )
      } yield typeBind :: constructorBinds
    case FRecordTypeDecl(FIdentifier(i), typ, fields) =>
      val record = toTypeRecord(fields.map(_.p))
      val rec = withRecType(i, typ, record)
      for {
        recordType <- Context.run(withTypeAbs(typ, rec))
        typeBind <- bindTypeAbb(i, recordType)
        constructor <- Context.run(
          buildRecordConstructor(i, typ, Left(fields.map(_.p)))
        )
        constructorBind <- bindTermAbb(toRecordConstructorId(i), constructor)
      } yield typeBind :: List(constructorBind)
    case FTupleTypeDecl(FIdentifier(i), typ, types) =>
      val tuple = toTupleTypeRecord(types)
      val rec = withRecType(i, typ, tuple)
      for {
        recordType <- Context.run(withTypeAbs(typ, rec))
        typeBind <- bindTypeAbb(i, recordType)
        constructor <- Context.run(buildRecordConstructor(i, typ, Right(types)))
        constructorBind <- bindTermAbb(toRecordConstructorId(i), constructor)
      } yield typeBind :: List(constructorBind)
    case FTypeAlias(FIdentifier(i), typ, t) =>
      val alias = toType(t)
      for {
        alias <- Context.run(withTypeAbs(typ, alias))
        typeBind <- bindTypeAbb(i, alias)
      } yield List(typeBind)
    case FFuncDecl(sig @ FFuncSig(FIdentifier(i), tp, _, _), exprs) => {
      for {
        func <- Context.run(buildFunc(tp, sig, exprs))
        funcBind <- bindTermAbb(i, func)
      } yield List(funcBind)
    }
    case FTypeFuncDecls(typeIdentifier, typ, functions) =>
      val param = FParam(
        FIdentifier("this"),
        FSimpleType(
          typeIdentifier,
          Some(
            typ
              .getOrElse(Seq())
              .map(tp => FSimpleType(FIdentifier(tp.i.value)))
          )
        )
      )
      val modifySignature = (sig: FFuncSig) =>
        FFuncSig(
          FIdentifier(s"!${sig.i.value}#${typeIdentifier.value}"),
          typ.map2(sig.tp)((tp1, tp2) => tp1 ++ tp2),
          sig.p.map(param +: _).orElse(Some(Seq(param))),
          sig.r
        )
      functions.toList
        .traverse(f => bind(FFuncDecl(modifySignature(f.sig), f.exprs)))
        .map(_.flatten)

    case _ => throw new Exception("not supported decl")
  }

  def bindTypeAbb(i: String, t: Type): ContextState[Bind] =
    Context.addName(i).map(Bind(_, TypeAbbBind(t)))

  def bindTermAbb(i: String, t: Term): ContextState[Bind] =
    Context.addName(i).map(Bind(_, TermAbbBind(t)))

  // # Bind # region_end

  // # Term # region_start

  def buildFunc(
      tp: FTypeParamClause,
      sig: FFuncSig,
      exprs: Seq[FExpr]
  ): ContextState[Term] = for {
    t <- withTermTypeAbs(tp)
    abs <- withFuncAbs(sig, toTermExpr(exprs.toList))
  } yield t(abs)

  def withFuncAbs(
      sig: FFuncSig,
      body: ContextState[Term]
  ): ContextState[Term] = sig.p match {
    case Some(params) =>
      val func = params.zipWithIndex
        .foldRight(body) { case ((p, index), acc) =>
          for {
            typ <- toType(p.t)
            variable <- Context.addName(p.i.value)
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
        _ <- Context.addName(WildcardName)
        typ <- toType(sig.r)
        term <- body
      } yield TermAbs(WildcardName, TypeUnit, term, Some(typ))
  }

  def toTermExpr(
      exprs: List[FExpr],
      letVariable: Option[(String, Option[FType])] = None
  ): ContextState[Term] = exprs match {
    case Nil => State.pure(TermUnit)
    case l @ FLetExpr(i, t, e) :: lexprs =>
      for {
        lt <- withTermLet(i.value, t, e.toList)
        le <- toTermExpr(lexprs)
      } yield lt(le)
    case h :: Nil => toTerm(h, letVariable)
    case _        => throw new Exception("invalid expr")
  }

  def withTermLet(
      i: String,
      t: Option[FType],
      expr: List[FExpr]
  ): ContextState[Term => Term] =
    for {
      t1 <- toTermExpr(expr, Some((i, t)))
      v <- Context.addName(i)
    } yield t2 => TermLet(v, t1, t2): Term

  def toTerm(
      e: FExpr,
      letVariable: Option[(String, Option[FType])] = None
  ): ContextState[Term] =
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
            c.p.toList.traverse(p => Context.run(toMatchCase(p, exprList)))
          })
        } yield TermMatch(me, mc.flatten)
      case FProj(e, projections) =>
        toTerm(e).map(t =>
          projections.foldLeft(t)((terms, proj) => TermProj(terms, proj.value))
        )

      case FAbs(bindings, Some(rType), expr) =>
        letVariable match {
          case Some((f, _)) =>
            val closure = withClosure(bindings.toList, toTermExpr(expr.toList))
            Context.run(
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
      case FVar(i) =>
        toTermVar(i).map(_ match {
          case Some(v) => v
          case None    => throw new Exception(s"Var not found $i")
        })
      case FBool(true)  => State.pure(TermTrue)
      case FBool(false) => State.pure(TermFalse)
      case FInt(i)      => State.pure(TermInt(i))
      case FFloat(f)    => State.pure(TermFloat(f))
      case FString(s)   => State.pure(TermString(s))
      case _            => throw new Exception("not supported expr")
    }

  def withClosure(
      params: List[FBinding],
      body: ContextState[Term]
  ): ContextState[Term] =
    params.foldRight(body) { case (FBinding(i, Some(t)), acc) =>
      for {
        typ <- toType(t)
        v <- Context.addName(i.value)
        term <- acc
      } yield TermClosure(v, Some(typ), term)
    }

  def withFixCombinator(
      name: String,
      params: List[FType],
      returnType: FType,
      func: ContextState[Term]
  ): ContextState[Term] = for {
    // The implicit param is added to represent the function we abstract,
    // in order to provide possibility for recursive call. Note that the
    // abstraction is wrapped in a fix combinator.
    ty <- toType(FFuncType(params, returnType))
    variable <- Context.addName(toRecAbsId(name))
    term <- func
    // NOTE: The abstraction is wrapped with a fix combinator to implement
    // recursion.
  } yield TermFix(TermAbs(variable, ty, term))

  def toTermOperator(
      func: String,
      e1: FExpr,
      e2: FExpr
  ): ContextState[Term] = for {
    funcVar <- toTermVar(func).map(_ match {
      case Some(v) => v
      case None    => throw new Exception(s"operator $func not found")
    })
    t1 <- toTerm(e1)
    t2 <- toTerm(e2)

  } yield TermApp(TermApp(funcVar, t1), t2)

  def toMatchCase(
      p: FPattern,
      e: List[FExpr]
  ): ContextState[(Pattern, Term)] = for {
    p <- toPattern(p)
    ce <- toTermExpr(e)
  } yield (p, ce)

  def toPattern(p: FPattern): ContextState[Pattern] = p match {
    case FIdentifierPattern(v, _) => State.pure(PatternNode(v))
    case FVariantOrRecordPattern(t, ps) =>
      for {
        np <- ps.toList.traverse(toPattern(_))
        vars <- np.traverse(i =>
          i match {
            case PatternNode(v, List()) => Context.addName(v)
            case PatternDefault         => State.pure[Context, String](WildcardName)
            case _                      => throw new Exception("not supported nested pattern")
          }
        )
      } yield PatternNode(t.value, vars)
    case FWildCardPattern => State.pure(PatternDefault)
    case FBool(true)      => State.pure(TermTrue)
    case FBool(false)     => State.pure(TermFalse)
    case FInt(i)          => State.pure(TermInt(i))
    case FFloat(f)        => State.pure(TermFloat(f))
    case FString(s)       => State.pure(TermString(s))
    case _                => throw new Exception("not supported case")
  }

  def toTermVar(i: String): ContextState[Option[Term]] =
    State { ctx =>
      Context
        .nameToIndex(ctx, i)
        .orElse(Context.nameToIndex(ctx, toRecAbsId(i))) match {
        case Some(index) => (ctx, Some(TermVar(index, ctx.length)))
        case None        => (ctx, None)
      }
    }

  def toRecAbsId(i: String): String = s"^$i"

  // # Term # region_end

  // # Type # region_start

  def toTypeVariant(v: Seq[FVariantTypeValue]): ContextState[Type] =
    v.toList
      .traverse(_ match {
        case FVariantTypeValue(FIdentifier(ti), None) =>
          State.pure[Context, (String, Type)]((ti, TypeUnit))
        case FVariantTypeValue(FIdentifier(ti), Some(Right(ts))) =>
          toTupleTypeRecord(ts).map((ti, _))
        case FVariantTypeValue(FIdentifier(ti), Some(Left(p))) =>
          toTypeRecord(p).map((ti, _))
      })
      .map(TypeVariant(_))

  def toTypeRecord(p: FParams): ContextState[Type] =
    p.toList.traverse(v => toType(v.t).map((v.i.value, _))).map(TypeRecord(_))

  def toTupleTypeRecord(ts: FTypes): ContextState[Type] =
    ts.toList.zipWithIndex
      .traverse(v => toType(v._1).map(((v._2 + 1).toString, _)))
      .map(TypeRecord(_))

  def withTypeAbs(
      tp: FTypeParamClause,
      typ: ContextState[Type]
  ): ContextState[Type] = tp match {
    case Some(params) =>
      val ids = params.map(_.i.value).toList
      for {
        _ <- ids.traverse(Context.addName(_))
        ty <- typ
      } yield ids.foldRight(ty)((p, acc) => TypeAbs(p, acc))
    case None => typ
  }

  def withRecType(
      i: String,
      tp: FTypeParamClause,
      typ: ContextState[Type]
  ): ContextState[Type] = for {
    ri <- Context.addName(toRecId(i))
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
  ): ContextState[Term] = v match {
    case FVariantTypeValue(FIdentifier(i), None) =>
      for {
        g <- withTermTypeAbs(tp)
        tag = toTermTag(variantName, tp, i, State.pure(TermUnit))
        fold <- withFold(variantName, tp, tag)
      } yield g(fold)
    case FVariantTypeValue(FIdentifier(i), Some(fields)) =>
      for {
        g <- withTermTypeAbs(tp)
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
  ): ContextState[Term] = for {
    g <- withTermTypeAbs(tp)
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
  ): ContextState[Term] =
    values
      .map { case (i, t) => (withTupleParamId(i), t) }
      .foldRight(body)((p, acc) =>
        for {
          typ <- toType(p._2)
          v <- Context.addName(p._1)
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

  // # Constructors # region_end

  def toType(t: FType): ContextState[Type] =
    t match {
      case FSimpleType(FIdentifier("i32"), None)  => State.pure(TypeInt)
      case FSimpleType(FIdentifier("f32"), None)  => State.pure(TypeFloat)
      case FSimpleType(FIdentifier("bool"), None) => State.pure(TypeBool)
      case FSimpleType(FIdentifier("str"), None)  => State.pure(TypeString)
      case FSimpleType(FIdentifier(i), None)      => toTypeVarOrId(i)
      case FSimpleType(FIdentifier(i), Some(tys)) =>
        tys.foldLeft(toTypeVarOrId(i))((acc, typ) =>
          for {
            t <- toType(typ)
            app <- acc
          } yield TypeApp(app, t)
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
      case _ => throw new Exception("not supported type")
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
