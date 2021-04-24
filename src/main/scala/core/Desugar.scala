package core

import cats.data.State
import cats.implicits._
import core.Context._
import parser.FuseLexicalParser._
import parser.FuseParser._
import parser.FuseTypesParser._
import parser.FuseExpressionParser._

object Desugar {
  def process(decls: List[FDecl]): State[Context, List[Bind]] =
    decls.traverse(bind(_)).map(_.flatten)

  // # Bind # region_start

  def bind(d: FDecl): State[Context, List[Bind]] = d match {
    case FVariantTypeDecl(FIdentifier(i), typ, values) =>
      for {
        abs <- withTypeAbs(typ)
        r <- withRecType(i, abs._1)
        t <- toTypeVariant(values)
        hb <- bindTypeAbb(i, abs._2(r(t)))
        tb <- buildVariantConstructors(i, typ, values.toList)
      } yield hb :: tb
    case FRecordTypeDecl(FIdentifier(i), typ, fields) =>
      for {
        abs <- withTypeAbs(typ)
        r <- withRecType(i, abs._1)
        t <- toTypeRecord(fields.map(_.p))
        hb <- bindTypeAbb(i, abs._2(r(t)))
        tb <- buildRecordConstructor(i, typ, Left(fields.map(_.p)))
      } yield hb :: List(tb)
    case FTupleTypeDecl(FIdentifier(i), typ, types) =>
      for {
        abs <- withTypeAbs(typ)
        r <- withRecType(i, abs._1)
        t <- toTupleTypeRecord(types)
        hb <- bindTypeAbb(i, abs._2(r(t)))
        tb <- buildRecordConstructor(i, typ, Right(types))
      } yield hb :: List(tb)
    case FTypeAlias(FIdentifier(i), typ, t) =>
      for {
        abs <- withTypeAbs(typ)
        a <- toType(t)
        b <- bindTypeAbb(i, abs._2(a))
      } yield List(b)
    case FFuncDecl(sig @ FFuncSig(FIdentifier(i), tp, _, _), exprs) => {
      for {
        t <- withTermTypeAbs(tp)
        abs <- withFuncAbs(sig)
        e <- toTermExpr(exprs.toList)
        b <- bindTermAbb(i, t(abs(e)))
      } yield List(b)
    }
    case _ => throw new Exception("not supported decl")
  }

  def bindTypeAbb(i: String, t: Type): State[Context, Bind] = for {
    _ <- addNameToContext(i)
  } yield Bind(i, TypeAbbBind(t))

  def bindTermAbb(i: String, t: Term): State[Context, Bind] = for {
    _ <- addNameToContext(i)
  } yield Bind(i, TermAbbBind(t))

  // # Bind # region_end

  // # Term # region_start

  def withFuncAbs(s: FFuncSig): State[Context, Term => Term] = State { ctx =>
    s.p match {
      case Some(params) =>
        // The implicit param is added to represent the function we abstract,
        // in order to provide possibility for recursive call. Note that the
        // abstraction is wrapped in a fix combinator.
        val funParam = FParam(
          FIdentifier(toRecAbsId(s.i.value)),
          FFuncType(params.map(_.t), s.r)
        )
        val paramsWithFun = funParam :: params.toList
        val c1 = paramsWithFun.foldLeft(ctx)((acc, p) =>
          Context.addName(acc, p.i.value)
        )
        val abs = (e: Term) =>
          paramsWithFun
            .foldRight((c1, e)) { case (p, (c, acc)) =>
              val ::(_, c2) = c
              // NOTE: The return type specified by the function signature
              // should be added to the last nested abstraction, since that one
              // is returnig the value for the function.
              val retType = c.length == c1.length match {
                case true  => Some(toType(s.r).runA(ctx).value)
                case false => None
              }
              val abs =
                TermAbs(p.i.value, toType(p.t).runA(c2).value, acc, retType)
              (c2, abs)
            }
            ._2
        // NOTE: The abstraction is wrapped with a fix combinator to implement
        // recursion.
        (c1, e => TermFix(abs(e)))
      case None =>
        val retType = toType(s.r).runA(ctx).value
        (ctx, e => TermAbs("_", TypeUnit, e, Some(retType)))
    }
  }

  def toTermExpr(e: List[FExpr]): State[Context, Term] =
    e.traverse(toTerm(_)).map(_.last)

  def toTerm(e: FExpr): State[Context, Term] =
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
      case FMultiplication(i1, i2) =>
        toTermOperator("&multiply", i1, i2)
      // TODO: Add other operators.
      case FAddition(i1, i2) =>
        toTermOperator("&add", i1, i2)
      case FSubtraction(i1, i2) =>
        toTermOperator("&sub", i1, i2)
      case FVar(i) =>
        State { ctx =>
          toTermVar(i, ctx) match {
            case Some(v) => (ctx, v)
            case None    => throw new Exception("Var not found")
          }
        }
      case FBool(true)  => State.pure(TermTrue)
      case FBool(false) => State.pure(TermFalse)
      case FInt(i)      => State.pure(TermInt(i))
      case FFloat(f)    => State.pure(TermFloat(f))
      case FString(s)   => State.pure(TermString(s))
      case _            => throw new Exception("not supported expr")
    }

  def toTermOperator(
      func: String,
      e1: FExpr,
      e2: FExpr
  ): State[Context, Term] = State { ctx =>
    // TODO: Handle the case when the func isn't found in the context.
    val funcVar = toTermVar(func, ctx).get
    val (c1, t1) = toTerm(e1).run(ctx).value
    val (c2, t2) = toTerm(e2).run(ctx).value
    (c2, TermApp(TermApp(funcVar, t1), t2))
  }

  def toMatchCase(
      p: FPattern,
      e: List[FExpr]
  ): State[Context, (Pattern, Term)] = for {
    p <- toPattern(p)
    ce <- toTermExpr(e)
  } yield (p, ce)

  def toPattern(p: FPattern): State[Context, Pattern] = p match {
    case FIdentifierPattern(v, _) => State.pure(PatternNode(v))
    case FVariantOrRecordPattern(t, ps) =>
      for {
        np <- ps.toList.traverse(toPattern(_))
        vars <- np.traverse(i =>
          i match {
            case PatternNode(v, List()) => addNameToContext(v)
            case PatternDefault         => State.pure[Context, String]("_")
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

  def toTermVar(i: String, ctx: Context): Option[TermVar] =
    Context
      .nameToIndex(ctx, i)
      .orElse(Context.nameToIndex(ctx, toRecAbsId(i))) match {
      case Some(indx) => Some(TermVar(indx, ctx.length))
      case None       => None
    }

  def toRecAbsId(i: String): String = s"^$i"

  // # Term # region_end

  // # Type # region_start

  def toTypeVariant(v: Seq[FVariantTypeValue]): State[Context, TypeVariant] =
    State { ctx =>
      val t = TypeVariant(
        v.foldRight(List[(String, Type)]())((field, acc) => {
          field match {
            case FVariantTypeValue(FIdentifier(ti), None) =>
              (ti, TypeUnit) :: acc
            case FVariantTypeValue(FIdentifier(ti), Some(Right(ts))) =>
              (ti, toTupleTypeRecord(ts).runA(ctx).value) :: acc
            case FVariantTypeValue(FIdentifier(ti), Some(Left(p))) =>
              (ti, toTypeRecord(p).runA(ctx).value) :: acc
          }
        })
      )
      (ctx, t)
    }

  def toTypeRecord(p: FParams): State[Context, TypeRecord] = State { ctx =>
    val t = TypeRecord(p.foldRight(List[(String, Type)]())((v, acc) => {
      (v.i.value, toType(v.t).runA(ctx).value) :: acc
    }))
    (ctx, t)
  }

  def toTupleTypeRecord(ts: FTypes): State[Context, TypeRecord] = State { ctx =>
    val t =
      TypeRecord(ts.zipWithIndex.foldRight(List[(String, Type)]())((v, acc) => {
        ((v._2 + 1).toString, toType(v._1).runA(ctx).value) :: acc
      }))
    (ctx, t)
  }

  def withTypeAbs(typ: FTypeParamClause): State[Context, (Kind, Type => Type)] =
    State { ctx =>
      typ match {
        case Some(p) => {
          val ids = p.map(_.i.value).toList
          // First add all the type variables in the context.
          val c1 = ids.foldLeft(ctx)((acc, n) => Context.addName(acc, n))
          // Calculate the kind of the recursive type by counting the number of
          // its type parameters. Kind arrow should be right associative.
          val knd = List
            .fill(ids.length + 1)(KindStar: Kind)
            .reduceRight(KindArrow(_, _))
          // Use the type to bulid type abstraction containg the type variables.
          (c1, (knd, t => ids.foldRight(t)((i, acc) => TypeAbs(i, acc))))
        }
        case None => (ctx, (KindStar, identity))
      }
    }

  def withRecType(i: String, k: Kind): State[Context, Type => TypeRec] = for {
    ri <- addNameToContext(toRecId(i))
  } yield t => TypeRec(ri, k, t)

  // # Type # region_end

  // # Constructors # region_start

  def buildVariantConstructors(
      name: String,
      tp: FTypeParamClause,
      t: List[FVariantTypeValue]
  ): State[Context, List[Bind]] =
    t.traverse(buildVariantConstructor(name, tp, _))

  def buildVariantConstructor(
      variantName: String,
      tp: FTypeParamClause,
      v: FVariantTypeValue
  ): State[Context, Bind] = v match {
    case FVariantTypeValue(FIdentifier(i), None) =>
      for {
        g <- withTermTypeAbs(tp)
        f <- withFold(variantName, tp)
        tag <- toTermTag(variantName, i, TermUnit)
        b <- bindTermAbb(i, g(f(tag)))
      } yield b
    case FVariantTypeValue(FIdentifier(i), Some(fields)) =>
      for {
        g <- withTermTypeAbs(tp)
        values = toRecordValues(fields)
        abs <- withRecordAbs(values)
        f <- withFold(variantName, tp)
        r <- toTermRecord(values)
        tag <- toTermTag(variantName, i, r)
        b <- bindTermAbb(i, g(abs(f(tag))))
      } yield b
  }

  def buildRecordConstructor(
      recordName: String,
      tp: FTypeParamClause,
      fields: Either[FParams, FTypes]
  ): State[Context, Bind] = for {
    g <- withTermTypeAbs(tp)
    values = toRecordValues(fields)
    abs <- withRecordAbs(values)
    f <- withFold(recordName, tp)
    r <- toTermRecord(values)
    b <- bindTermAbb(toRecordConstructorId(recordName), g(abs(f(r))))
  } yield b

  def toTermRecord(values: List[(String, FType)]): State[Context, TermRecord] =
    State { ctx =>
      val term = TermRecord(values.foldRight(List[(String, Term)]()) {
        case ((n, t), acc) =>
          (n, toTermVar(withTupleParamId(n), ctx).get) :: acc
      })
      (ctx, term)
    }

  def toTermTag(
      name: String,
      tag: String,
      term: Term
  ): State[Context, Term] = State { ctx =>
    (ctx, TermTag(tag, term, toTypeVarOrId(ctx, toRecId(name))))
  }

  def toRecordValues(fields: Either[FParams, FTypes]) =
    fields match {
      case Left(params) => params.toList.map(f => (f.i.value, f.t))
      case Right(types) =>
        types.toList.zipWithIndex.map(v => ((v._2 + 1).toString, v._1))
    }

  def withTermTypeAbs(typ: FTypeParamClause): State[Context, Term => Term] =
    typ match {
      case Some(p) =>
        val ids = p.map(_.i.value).toList
        for {
          _ <- ids.traverse(addNameToContext(_))
        } yield t => ids.foldRight(t)((i, acc) => TermTAbs(i, acc))
      case None => State.pure(identity)
    }

  def withRecordAbs(
      values: List[(String, FType)]
  ): State[Context, Term => Term] = {
    State { ctx =>
      val params = values.map { case (i, t) => (withTupleParamId(i), t) }
      // Then add all the parameters as term variables in the context.
      val c1 = params.traverse(p => addNameToContext(p._1)).runS(ctx).value
      // Use the term to create term abstractions – equal to a function.
      val termAbsFunc = (term: Term) =>
        params
          .foldRight((c1, term)) { case ((v, t), (c, acc)) =>
            val ::(_, c2) = c
            val abs = TermAbs(v, toType(t).runA(c2).value, acc)
            (c2, abs)
          }
          ._2
      (c1, termAbsFunc)
    }
  }

  def withFold(
      name: String,
      tp: FTypeParamClause
  ): State[Context, Term => TermApp] = State { ctx =>
    tp match {
      case None => (ctx, t => TermApp(TermFold(toTypeVarOrId(ctx, name)), t))
      case Some(tys) =>
        val t = toTypeVarOrId(ctx, name)
        val typeApp = tys.foldLeft(t)((ta, tp) =>
          TypeApp(ta, toTypeVarOrId(ctx, tp.i.value))
        )
        (ctx, t => TermApp(TermFold(typeApp), t))
    }
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

  def toType(t: FType): State[Context, Type] = State { ctx =>
    t match {
      case FSimpleType(FIdentifier("i32"), None)  => (ctx, TypeInt)
      case FSimpleType(FIdentifier("f32"), None)  => (ctx, TypeFloat)
      case FSimpleType(FIdentifier("bool"), None) => (ctx, TypeBool)
      case FSimpleType(FIdentifier("str"), None)  => (ctx, TypeString)
      case FSimpleType(FIdentifier(i), None)      => (ctx, toTypeVarOrId(ctx, i))
      case FSimpleType(FIdentifier(i), Some(tys)) =>
        val t = toTypeVarOrId(ctx, i)
        // Type application should be left associative.
        val app =
          tys.foldLeft(t)((ta, tp) => TypeApp(ta, toType(tp).runA(ctx).value))
        (ctx, app)
      case FFuncType(ts, t1) =>
        val i = ts.map(toType(_).runA(ctx).value).toList
        val o = toType(t1).runA(ctx).value
        val t = i match {
          case Nil => TypeArrow(TypeUnit, o)
          // Type abstractions (arrow) should be right associative.
          case _ => (i :+ o).reduceRight(TypeArrow(_, _))
        }
        (ctx, t)
      case _ => throw new Exception("not supported type")
    }
  }

  def toTypeVarOrId(ctx: Context, i: String): Type =
    toCtxIndex(ctx, i) match {
      case Some(index) => TypeVar(index, ctx.length)
      case None        => TypeId(i)
    }

  def toCtxIndex(ctx: Context, i: String): Option[Int] =
    Context.nameToIndex(ctx, i).orElse(Context.nameToIndex(ctx, toRecId(i)))

  def addNameToContext(n: String): State[Context, String] =
    State(ctx => (Context.addName(ctx, n), n))

  def toRecId(i: String) = s"@$i"

}
