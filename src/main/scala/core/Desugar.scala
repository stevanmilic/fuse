package core

import cats.data.State
import cats.implicits._
import core.Context._
import parser.FuseLexicalParser._
import parser.FuseParser._
import parser.FuseTypesParser._

object Desugar {
  def process(decls: List[FDecl]): State[Context, List[Bind]] =
    decls.traverse(bind(_))

  def bind(d: FDecl): State[Context, Bind] = State { ctx =>
    d match {
      case FVariantTypeDecl(FIdentifier(i), typ, values) => {
        val t = values.toStream
          .map(_.t)
          .flatten
          .flatMap(_ match {
            case Right(r) => r
            case Left(p)  => p.map(_.t)
          })
        val (c1, variant) = withTypeAbs(
          typ,
          withRecType(i, t, toTypeVariant(values))
        )(ctx)
        (Context.addName(c1, i), Bind(i, TypeAbbBind(variant)))
      }
      case FRecordTypeDecl(FIdentifier(i), typ, fields) => {
        val p = fields.map(_.p)
        val (c1, record) = withTypeAbs(
          typ,
          withRecType(i, p.map(_.t), toTypeRecord(p))
        )(ctx)
        (Context.addName(c1, i), Bind(i, TypeAbbBind(record)))
      }
      case FTupleTypeDecl(FIdentifier(i), typ, types) => {
        val (c1, record) = withTypeAbs(
          typ,
          withRecType(i, types, toTupleTypeRecord(types))
        )(ctx)
        (Context.addName(c1, i), Bind(i, TypeAbbBind(record)))
      }
      case FTypeAlias(FIdentifier(i), typ, t) =>
        val (c1, abb) = withTypeAbs(
          typ,
          withRecType(i, Seq(t), toType(t))
        )(ctx)
        (Context.addName(c1, i), Bind(i, TypeAbbBind(abb)))
      case _ => throw new Exception("not supported decl")
    }
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
        val c1 = ids.foldLeft(ctx)((acc, n) => (Context.addName(acc, n)))
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
