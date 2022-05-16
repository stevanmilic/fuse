package core

import core.Bindings.*
import core.Types.*
import parser.Info.*

object Shifting {

  type ShiftVarFunc = (Info, Int, Int, Int) => Type

  def bindingShift(d: Int, b: Binding): Binding = b match {
    case NameBind              => NameBind
    case TypeEMarkBind         => TypeEMarkBind
    case TypeEFreeBind         => TypeEFreeBind
    case TempVarBind           => TempVarBind
    case TypeESolutionBind(ty) => TypeESolutionBind(typeShift(d, ty))
    case t @ TypeVarBind(_)    => t
    case VarBind(ty)           => VarBind(typeShift(d, ty))
    case TypeAbbBind(ty, k)    => TypeAbbBind(typeShift(d, ty), k)
    case TermAbbBind(term, Some(ty)) =>
      TermAbbBind(term, Some(typeShift(d, ty)))
    case t @ TermAbbBind(_, None) => t
  }

  def typeShift(d: Int, ty: Type): Type =
    typeShiftAbove(d, 0, ty)

  def typeShiftAbove(d: Int, c: Int, ty: Type): Type =
    typeMap(
      (info, c, k, n) =>
        if (k >= c) TypeVar(info, k + d, n + d) else TypeVar(info, k, n + d),
      c,
      ty
    )

  def typeSubstituteTop(tyS: Type, tyT: Type): Type =
    typeShift(-1, typeSubstitute(typeShift(1, tyS), 0, tyT))

  def typeSubstitute(tyS: Type, c: Int, tyT: Type): Type =
    typeMap(
      (info, j, x, n) => if (x == j) typeShift(j, tyS) else TypeVar(info, x, n),
      c,
      tyT
    )

  def typeMap(onVar: ShiftVarFunc, c: Int, t: Type): Type = {
    def iter(c: Int, tyT: Type): Type = tyT match {
      case TypeVar(info, x, n) => onVar(info, c, x, n)
      case TypeEVar(_, _)      => tyT
      case TypeId(_, _)        => tyT
      case TypeRecord(info, fieldTypes) =>
        TypeRecord(
          info,
          fieldTypes.map { case (l, tyTi) => (l, iter(c, tyTi)) }
        )
      case TypeVariant(info, fieldTypes) =>
        TypeVariant(
          info,
          fieldTypes.map { case (l, tyTi) => (l, iter(c, tyTi)) }
        )
      case TypeArrow(info, tyT1, tyT2) =>
        TypeArrow(info, iter(c, tyT1), iter(c, tyT2))
      case TypeRec(info, x, k, tyTi) => TypeRec(info, x, k, iter(c + 1, tyTi))
      case TypeAll(info, x, k, tyTi) => TypeAll(info, x, k, iter(c + 1, tyTi))
      case TypeAbs(info, x, tyTi)    => TypeAbs(info, x, iter(c + 1, tyTi))
      case TypeApp(info, tyT1, tyT2) =>
        TypeApp(info, iter(c, tyT1), iter(c, tyT2))
      case TypeString(info) => TypeString(info)
      case TypeUnit(info)   => TypeUnit(info)
      case TypeInt(info)    => TypeInt(info)
      case TypeFloat(info)  => TypeFloat(info)
      case TypeBool(info)   => TypeBool(info)
    }
    iter(c, t)
  }
}
