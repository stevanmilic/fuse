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
        abs <- withTypeAbs(typ, rec)
        hb <- bindTypeAbb(i, abs)
        tb <- buildVariantConstructors(i, typ, values.toList)
      } yield hb :: tb
    case FRecordTypeDecl(FIdentifier(i), typ, fields) =>
      val record = toTypeRecord(fields.map(_.p))
      val rec = withRecType(i, typ, record)
      for {
        abs <- withTypeAbs(typ, rec)
        hb <- bindTypeAbb(i, abs)
        tb <- buildRecordConstructor(i, typ, Left(fields.map(_.p)))
      } yield hb :: List(tb)
    case FTupleTypeDecl(FIdentifier(i), typ, types) =>
      val tuple = toTupleTypeRecord(types)
      val rec = withRecType(i, typ, tuple)
      for {
        abs <- withTypeAbs(typ, rec)
        hb <- bindTypeAbb(i, abs)
        tb <- buildRecordConstructor(i, typ, Right(types))
      } yield hb :: List(tb)
    case FTypeAlias(FIdentifier(i), typ, t) =>
      val alias = toType(t)
      for {
        abs <- withTypeAbs(typ, alias)
        b <- bindTypeAbb(i, abs)
      } yield List(b)
    case FFuncDecl(sig @ FFuncSig(FIdentifier(i), tp, _, _), exprs) => {
      for {
        t <- withTermTypeAbs(tp)
        abs <- withFuncAbs(sig, toTermExpr(exprs.toList))
        b <- bindTermAbb(i, t(abs))
      } yield List(b)
    }
    case _ => throw new Exception("not supported decl")
  }

  def bindTypeAbb(i: String, t: Type): ContextState[Bind] =
    Context.addName(i).map(Bind(_, TypeAbbBind(t)))

  def bindTermAbb(i: String, t: Term): ContextState[Bind] =
    Context.addName(i).map(Bind(_, TermAbbBind(t)))

  // # Bind # region_end

  // # Term # region_start

  def withFuncAbs(
      s: FFuncSig,
      body: ContextState[Term]
  ): ContextState[Term] = s.p match {
    case Some(params) =>
      // The implicit param is added to represent the function we abstract,
      // in order to provide possibility for recursive call. Note that the
      // abstraction is wrapped in a fix combinator.
      val funParam = FParam(
        FIdentifier(toRecAbsId(s.i.value)),
        FFuncType(params.map(_.t), s.r)
      )
      val paramsWithFun = funParam :: params.toList
      paramsWithFun.zipWithIndex
        .foldRight(body) { case ((p, indx), acc) =>
          for {
            typ <- toType(p.t)
            variable <- Context.addName(p.i.value)
            term <- acc
            retType <- toType(s.r)
            // NOTE: The return type specified by the function signature
            // should be added to the last nested abstraction, since that one
            // is returnig the value for the function.
            retVal =
              if (indx == paramsWithFun.length - 1) Some(retType) else None
          } yield TermAbs(variable, typ, term, retVal)
        }
        // NOTE: The abstraction is wrapped with a fix combinator to implement
        // recursion.
        .map(TermFix(_))
    case None =>
      for {
        typ <- toType(s.r)
        term <- body
      } yield TermAbs(WildcardName, TypeUnit, term, Some(typ))
  }

  def toTermExpr(exprs: List[FExpr]): ContextState[Term] = exprs match {
    case Nil => State.pure(TermUnit)
    case l @ FLetExpr(i, _, e) :: lexprs =>
      for {
        lt <- withTermLet(i.value, e.toList)
        le <- toTermExpr(lexprs)
      } yield lt(le)
    case h :: Nil => toTerm(h)
    case _        => throw new Exception("invalid expr")
  }

  def withTermLet(i: String, expr: List[FExpr]): ContextState[Term => Term] =
    for {
      t1 <- toTermExpr(expr)
      v <- Context.addName(i)
    } yield t2 => TermLet(v, t1, t2): Term

  def toTerm(e: FExpr): ContextState[Term] =
    e match {
      case FApp(e, args) =>
        val v = e :: args.toList.flatten.flatten
        v.traverse(toTerm(_)).map(_.reduceLeft(TermApp(_, _)))
      case FMatch(e, cases) =>
        for {
          me <- toTerm(e)
          mc <- cases.toList.traverse(c => {
            val exprList = c.e.toList
            c.p.toList.traverse(toMatchCase(_, exprList))
          })
        } yield TermMatch(me, mc.flatten)
      case FAbs(bindings, expr) =>
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
          case None    => throw new Exception("Var not found")
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
        case Some(indx) => (ctx, Some(TermVar(indx, ctx.length)))
        case None       => (ctx, None)
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
      params.foldRight(typ)((p, acc) =>
        for {
          v <- Context.addName(p.i.value)
          t <- acc
        } yield TypeAbs(v, t)
      )
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

  def buildVariantConstructors(
      name: String,
      tp: FTypeParamClause,
      t: List[FVariantTypeValue]
  ): ContextState[List[Bind]] =
    t.traverse(buildVariantConstructor(name, tp, _))

  def buildVariantConstructor(
      variantName: String,
      tp: FTypeParamClause,
      v: FVariantTypeValue
  ): ContextState[Bind] = v match {
    case FVariantTypeValue(FIdentifier(i), None) =>
      for {
        g <- withTermTypeAbs(tp)
        tag = toTermTag(variantName, i, State.pure(TermUnit))
        fold <- withFold(variantName, tp, tag)
        b <- bindTermAbb(i, g(fold))
      } yield b
    case FVariantTypeValue(FIdentifier(i), Some(fields)) =>
      for {
        g <- withTermTypeAbs(tp)
        values = toRecordValues(fields)
        r = toTermRecord(values)
        tag = toTermTag(variantName, i, r)
        fold = withFold(variantName, tp, tag)
        abs <- withRecordAbs(values, fold)
        b <- bindTermAbb(i, g(abs))
      } yield b
  }

  def buildRecordConstructor(
      recordName: String,
      tp: FTypeParamClause,
      fields: Either[FParams, FTypes]
  ): ContextState[Bind] = for {
    g <- withTermTypeAbs(tp)
    values = toRecordValues(fields)
    r = toTermRecord(values)
    fold = withFold(recordName, tp, r)
    abs <- withRecordAbs(values, fold)
    b <- bindTermAbb(toRecordConstructorId(recordName), g(abs))
  } yield b

  def toTermRecord(values: List[(String, FType)]): ContextState[Term] =
    values
      .traverse { case (n, t) =>
        toTermVar(withTupleParamId(n)).map(term => (n, term.get))
      }
      .map(TermRecord(_))

  def toTermTag(
      name: String,
      tag: String,
      body: ContextState[Term]
  ): ContextState[Term] = for {
    term <- body
    typ <- toTypeVarOrId(toRecId(name))
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
  ): ContextState[Term] = tp match {
    case None =>
      for {
        typ <- toTypeVarOrId(name)
        term <- body
      } yield TermApp(TermFold(typ), term)
    case Some(tys) =>
      tys
        .foldLeft(toTypeVarOrId(name))((acc, tp) =>
          for {
            typ <- toTypeVarOrId(tp.i.value)
            app <- acc
          } yield TypeApp(app, typ)
        )
        .map2(body)((typ, term) => TermApp(TermFold(typ), term))
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
