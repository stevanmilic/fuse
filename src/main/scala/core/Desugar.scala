package core

import parser.FuseParser._
import parser.FuseLexicalParser._
import parser.FuseTypesParser._
import core.Context._

object Desugar {
  val RecursiveParam = "P"

  def process(decls: Seq[FDecl]): (List[Bind], Context) = {
    decls.foldLeft((List[Bind](), Context.empty))((acc, d) => {
      val (b, c) = bind(d, acc._2)
      (b :: acc._1, c)
    })
  }

  def bind(d: FDecl, c: Context): (Bind, Context) = d match {
    case FVariantTypeDecl(FIdentifier(i), None, values) => {
      val t = values.toStream
        .map(_.t)
        .flatten
        .flatMap(_ match {
          case Right(r) => r
          case Left(p)  => p.map(_.t)
        })
      val (c1, variant) =
        handleRecursiveType(c, i, t, ctx => toTypeVariant(ctx, values))
      (Bind(i, TypeAbbBind(variant)), Context.addName(c1, i))
    }
    case FRecordTypeDecl(FIdentifier(i), None, fields) => {
      val p = fields.map(_.p)
      val (c1, record) =
        handleRecursiveType(c, i, p.map(_.t), ctx => toTypeRecord(ctx, p))
      (Bind(i, TypeAbbBind(record)), Context.addName(c1, i))
    }
    case FTupleTypeDecl(FIdentifier(i), None, types) => {
      val (c1, record) =
        handleRecursiveType(c, i, types, ctx => toTupleTypeRecord(ctx, types))
      (Bind(i, TypeAbbBind(record)), Context.addName(c1, i))
    }
    case FTypeAlias(FIdentifier(i), None, t) =>
      val (c1, abb) =
        handleRecursiveType(c, i, Seq(t), ctx => toType(ctx, t))
      (Bind(i, TypeAbbBind(abb)), Context.addName(c1, i))
    case _ => throw new Exception("not supported decl")
  }

  def toTypeVariant(
      c: Context,
      v: Seq[FVariantTypeValue]
  ): TypeVariant =
    TypeVariant(
      v.foldRight(List[(String, Type)]())((field, acc) => {
        field match {
          case FVariantTypeValue(FIdentifier(ti), None) =>
            (ti, TypeUnit) :: acc
          case FVariantTypeValue(FIdentifier(ti), Some(Right(ts))) =>
            (ti, toTupleTypeRecord(c, ts)) :: acc
          case FVariantTypeValue(FIdentifier(ti), Some(Left(p))) =>
            (ti, toTypeRecord(c, p)) :: acc
          case _ => acc
        }
      })
    )

  def toTypeRecord(c: Context, p: FParams): TypeRecord =
    TypeRecord(p.foldRight(List[(String, Type)]())((v, acc) => {
      (v.i.value, toType(c, v.t)) :: acc
    }))

  def toTupleTypeRecord(c: Context, ts: FTypes): TypeRecord =
    TypeRecord(ts.zipWithIndex.foldRight(List[(String, Type)]())((v, acc) => {
      ((v._2 + 1).toString, toType(c, v._1)) :: acc
    }))

  def handleRecursiveType(
      c: Context,
      i: String,
      types: FTypes,
      f: Context => Type
  ): (Context, Type) = isIdentifierInTypes(i, types) match {
    case true => {
      val c1 = Context.addName(c, RecursiveParam)
      (c1, TypeRec(RecursiveParam, f(c1)))
    }
    case false => (c, f(c))
  }

  def isIdentifierInTypes(identifier: String, types: FTypes): Boolean =
    types.foldRight(false)((t, acc) => {
      val b = t match {
        case FSimpleType(FIdentifier(ti), None) => identifier == ti
        case FSimpleType(FIdentifier(ti), Some(Seq(ts))) =>
          identifier == ti || isIdentifierInTypes(identifier, Seq(ts))
        case FTupleType(ts) => isIdentifierInTypes(identifier, types)
        case FFuncType(ts, t) =>
          isIdentifierInTypes(identifier, ts) || isIdentifierInTypes(
            identifier,
            Seq(t)
          )
      }
      b || acc
    })

  def toType(c: Context, t: FType): Type = t match {
    case FSimpleType(FIdentifier("i32"), None) =>
      TypeInt
    case FSimpleType(FIdentifier("f32"), None) =>
      TypeFloat
    case FSimpleType(FIdentifier("bool"), None) =>
      TypeBool
    case FSimpleType(FIdentifier("str"), None) =>
      TypeString
    case FSimpleType(FIdentifier(i), None) =>
      Context.nameToIndex(c, i) match {
        case Some(index) => TypeVar(index, c.length)
        case None        => TypeId(i)
      }
    case FFuncType(ts, t1) => {
      def typesToArrowInput(l: List[Type]): Type = l match {
        case h :: Nil => h
        // TODO: Check if associativity is good.
        case h :: t => TypeArrow(h, typesToArrowInput(t))
        case Nil    => TypeUnit
      }
      val i = ts.map(toType(c, _)).toList
      val o = toType(c, t1)
      TypeArrow(typesToArrowInput(i), o)
    }
    case _ => throw new Exception("not supported type")
  }

}
