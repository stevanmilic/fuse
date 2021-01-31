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

  def bind(d: FDecl): State[Context, List[Bind]] = State { ctx =>
    d match {
      case FVariantTypeDecl(FIdentifier(i), typ, values) =>
        val t = values.toStream
          .map(_.t)
          .flatten
          .flatMap(_ match {
            case Right(r) => r
            case Left(p)  => p.map(_.t)
          })
        val rt = withRecType(i, t, toTypeVariant(values))
        val (c1, variant) = withTypeAbs(typ, rt)(ctx)
        // TODO: Add bindings for variant data constructors.
        (Context.addName(c1, i), List(Bind(i, TypeAbbBind(variant))))
      case FRecordTypeDecl(FIdentifier(i), typ, fields) =>
        val p = fields.map(_.p)
        val rt = withRecType(i, p.map(_.t), toTypeRecord(p))
        val (c1, record) = withTypeAbs(typ, rt)(ctx)
        (Context.addName(c1, i), List(Bind(i, TypeAbbBind(record))))
      case FTupleTypeDecl(FIdentifier(i), typ, types) =>
        val rt = withRecType(i, types, toTupleTypeRecord(types))
        val (c1, record) = withTypeAbs(typ, rt)(ctx)
        (Context.addName(c1, i), List(Bind(i, TypeAbbBind(record))))
      case FTypeAlias(FIdentifier(i), typ, t) =>
        val rt = withRecType(i, Seq(t), toType(t))
        val (c1, abb) = withTypeAbs(typ, rt)(ctx)
        (Context.addName(c1, i), List(Bind(i, TypeAbbBind(abb))))
      case FFuncDecl(sig @ FFuncSig(FIdentifier(i), _, _, _), exprs) => {
        val f = toTermExpr(exprs)
        val (c1, term) = toTermAbs(sig, f)(ctx)
        (Context.addName(c1, i), List(Bind(i, TermAbbBind(term))))
      }
      case _ => throw new Exception("not supported decl")
    }
  }

  def toTermAbs(
      s: FFuncSig,
      f: Context => (Context, Term)
  ): Context => (Context, TermAbs) = ctx => {
    val retType = toType(s.r)(ctx)
    s.p match {
      case Some(params) =>
        val c1 =
          params.foldLeft(ctx)((acc, p) => Context.addName(acc, p.i.value))
        val (c2, e) = f(c1)
        val ::(h, t) = params
        val term =
          t.foldRight(e)((p, acc) => TermAbs("_", toType(p.t)(c2), acc, None))
        (c2, TermAbs(s.i.value, toType(h.t)(c2), term, Some(retType)))
      case None =>
        val (c1, e) = f(ctx)
        (c1, TermAbs(s.i.value, TypeUnit, e, Some(retType)))
    }
  }

  def toTermExpr(e: Seq[FExpr]): Context => (Context, Term) = ctx =>
    e.foldLeft((ctx, TermUnit: Term)) { case ((c, _), e) => toTerm(e)(c) }

  def toTerm(e: FExpr): Context => (Context, Term) = ctx =>
    e match {
      case FApp(e, args) =>
        // TODO: Use the State monad and fold the context too here.
        val termArgs = args.flatten.flatten.map(toTerm(_)(ctx)._2).toList
        val (c1, f) = toTerm(e)(ctx)
        (c1, (f :: termArgs).reduceLeft(TermApp(_, _)))
      case FMultiplication(i1, i2) =>
        toTermOperator("int_mp", ctx, i1, i2)
      // TODO: Add other operators.
      case FAddition(i1, i2) =>
        toTermOperator("int_add", ctx, i1, i2)
      case FVar(i) =>
        toTermVar(i, ctx) match {
          case Some(v) => (ctx, v)
          case None    => throw new Exception("Var not found")
        }
      case FBool(true)  => (ctx, TermTrue)
      case FBool(false) => (ctx, TermFalse)
      case FInt(i)      => (ctx, TermInt(i))
      case FFloat(f)    => (ctx, TermFloat(f))
      case _            => throw new Exception("not supported expr")
    }

  def toTermOperator(
      func: String,
      ctx: Context,
      e1: FExpr,
      e2: FExpr
  ): (Context, TermApp) = {
    // TODO: Handle the case when the func isn't found in the context.
    val funcVar = toTermVar(func, ctx).get
    (
      ctx,
      TermApp(
        TermApp(funcVar, toTerm(e1)(ctx)._2),
        toTerm(e2)(ctx)._2
      )
    )
  }

  def toTermVar(i: String, c: Context): Option[TermVar] =
    Context.nameToIndex(c, i) match {
      case Some(indx) => Some(TermVar(indx, c.length))
      case None       => None
    }

  def toTypeVariant(
      v: Seq[FVariantTypeValue]
  ): Context => TypeVariant = ctx =>
    TypeVariant(
      v.foldRight(List[(String, Type)]())((field, acc) => {
        field match {
          case FVariantTypeValue(FIdentifier(ti), None) =>
            (ti, TypeUnit) :: acc
          case FVariantTypeValue(FIdentifier(ti), Some(Right(ts))) =>
            (ti, toTupleTypeRecord(ts)(ctx)) :: acc
          case FVariantTypeValue(FIdentifier(ti), Some(Left(p))) =>
            (ti, toTypeRecord(p)(ctx)) :: acc
        }
      })
    )

  def toTypeRecord(p: FParams): Context => TypeRecord = ctx =>
    TypeRecord(p.foldRight(List[(String, Type)]())((v, acc) => {
      (v.i.value, toType(v.t)(ctx)) :: acc
    }))

  def toTupleTypeRecord(ts: FTypes): Context => TypeRecord = ctx =>
    TypeRecord(ts.zipWithIndex.foldRight(List[(String, Type)]())((v, acc) => {
      ((v._2 + 1).toString, toType(v._1)(ctx)) :: acc
    }))

  def withTypeAbs(
      typ: FTypeParamClause,
      f: (Context, Kind) => (Context, Type)
  ): Context => (Context, Type) = ctx =>
    typ match {
      case Some(p) => {
        val ids = p.map(_.i.value).toList
        // First add all the type variables in the context.
        val c1 = ids.foldLeft(ctx)((acc, n) => Context.addName(acc, n))
        // Calculate the kind of the recursive type by counting the number of
        // its type parameters. Kind arrow should be right associative.
        val knd =
          List.fill(ids.length + 1)(KindStar: Kind).reduceRight(KindArrow(_, _))
        // Then determine the underlying type.
        val (c2, t) = f(c1, knd)
        // Use the type to bulid type abstraction containg the type variables.
        (c2, ids.foldRight(t)((i, acc) => TypeAbs(i, acc)))
      }
      case None => f(ctx, KindStar)
    }

  def withRecType(
      i: String,
      types: FTypes,
      f: Context => Type
  ): (Context, Kind) => (Context, Type) = (ctx, knd) =>
    isIdentifierInTypes(i, types) match {
      case true => {
        val ri = toRecId(i)
        val c1 = Context.addName(ctx, ri)
        (c1, TypeRec(ri, knd, f(c1)))
      }
      case false => (ctx, f(ctx))
    }

  def isIdentifierInTypes(identifier: String, types: FTypes): Boolean =
    types.foldRight(false)((t, acc) => {
      val b = t match {
        case FSimpleType(FIdentifier(ti), None) => identifier == ti
        case FSimpleType(FIdentifier(ti), Some(ts)) =>
          identifier == ti || isIdentifierInTypes(identifier, ts)
        case FTupleType(ts) => isIdentifierInTypes(identifier, types)
        case FFuncType(ts, t) =>
          isIdentifierInTypes(identifier, ts) || isIdentifierInTypes(
            identifier,
            Seq(t)
          )
      }
      b || acc
    })

  def toType(t: FType): Context => Type = ctx =>
    t match {
      case FSimpleType(FIdentifier("i32"), None) =>
        TypeInt
      case FSimpleType(FIdentifier("f32"), None) =>
        TypeFloat
      case FSimpleType(FIdentifier("bool"), None) =>
        TypeBool
      case FSimpleType(FIdentifier("str"), None) =>
        TypeString
      case FSimpleType(FIdentifier(i), None) =>
        toCtxIndex(ctx, i) match {
          case Some(index) => TypeVar(index, ctx.length)
          case None        => TypeId(i)
        }
      case FSimpleType(FIdentifier(i), Some(tys)) => {
        val t = toCtxIndex(ctx, i) match {
          case Some(index) => TypeVar(index, ctx.length)
          case None        => TypeId(i)
        }
        // Type application should be left associative.
        tys.foldLeft(t)((ta, tp) => TypeApp(ta, toType(tp)(ctx)))
      }
      case FFuncType(ts, t1) => {
        val i = ts.map(toType(_)(ctx)).toList
        val o = toType(t1)(ctx)
        i match {
          case Nil => TypeArrow(TypeUnit, o)
          // Type abstractions (arrow) should be right associative.
          case _ => (i :+ o).reduceRight(TypeArrow(_, _))
        }
      }
      case _ => throw new Exception("not supported type")
    }

  def toCtxIndex(ctx: Context, i: String): Option[Int] =
    Context.nameToIndex(ctx, i).orElse(Context.nameToIndex(ctx, toRecId(i)))

  def toRecId(i: String) = s"@$i"

}
