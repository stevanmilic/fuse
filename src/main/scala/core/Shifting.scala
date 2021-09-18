package core

import core.Bindings._
import core.Types._

object Shifting {

  type ShiftVarFunc = (Int, Int, Int) => Type

  def bindingShift(d: Int, b: Binding): Binding = b match {
    case NameBind           => NameBind
    case t @ TypeVarBind(_) => t
    case VarBind(ty)        => VarBind(typeShift(d, ty))
    case TypeAbbBind(ty, k) => TypeAbbBind(typeShift(d, ty), k)
    case TermAbbBind(term, Some(ty)) =>
      TermAbbBind(term, Some(typeShift(d, ty)))
    case t @ TermAbbBind(_, None) => t
  }

  def typeShift(d: Int, ty: Type): Type =
    typeShiftAbove(d, 0, ty)

  def typeShiftAbove(d: Int, c: Int, ty: Type): Type =
    typeMap(
      (c, k, n) => if (k >= c) TypeVar(k + d, n + d) else TypeVar(k, n + d),
      c,
      ty
    )

  def typeSubstituteTop(tyS: Type, tyT: Type): Type =
    typeShift(-1, typeSubstitute(typeShift(1, tyS), 0, tyT))

  def typeSubstitute(tyS: Type, c: Int, tyT: Type): Type =
    typeMap(
      (j, x, n) => if (x == j) typeShift(j, tyS) else TypeVar(x, n),
      c,
      tyT
    )

  def typeMap(onVar: ShiftVarFunc, c: Int, t: Type): Type = {
    def iter(c: Int, tyT: Type): Type = tyT match {
      case TypeVar(x, n) => onVar(c, x, n)
      case TypeId(_)     => tyT
      case TypeRecord(fieldTypes) =>
        TypeRecord(fieldTypes.map { case (l, tyTi) => (l, iter(c, tyTi)) })
      case TypeVariant(fieldTypes) =>
        TypeVariant(fieldTypes.map { case (l, tyTi) => (l, iter(c, tyTi)) })
      case TypeArrow(tyT1, tyT2) => TypeArrow(iter(c, tyT1), iter(c, tyT2))
      case TypeRec(x, k, tyTi)   => TypeRec(x, k, iter(c + 1, tyTi))
      case TypeAll(x, k, tyTi)   => TypeAll(x, k, iter(c + 1, tyTi))
      case TypeAbs(x, tyTi)      => TypeAbs(x, iter(c + 1, tyTi))
      case TypeApp(tyT1, tyT2)   => TypeApp(iter(c, tyT1), iter(c, tyT2))
      case TypeString            => TypeString
      case TypeUnit              => TypeUnit
      case TypeInt               => TypeInt
      case TypeFloat             => TypeFloat
      case TypeBool              => TypeBool
    }
    iter(c, t)
  }

}
