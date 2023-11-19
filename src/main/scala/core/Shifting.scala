package core

import core.Bindings.*
import core.Types.*
import core.Terms.*
import parser.Info.*

object Shifting {

  type ShiftVarFunc[T] = (Info, Int, Int, Int) => T

  def bindingShift(d: Int, b: Binding): Binding = b match {
    case NameBind              => NameBind
    case TypeEMarkBind         => TypeEMarkBind
    case TypeEFreeBind         => TypeEFreeBind
    case TempVarBind           => TempVarBind
    case TypeESolutionBind(ty) => TypeESolutionBind(typeShift(d, ty))
    case t @ TypeVarBind(_, _) => t
    case VarBind(ty)           => VarBind(typeShift(d, ty))
    case TypeAbbBind(ty, k)    => TypeAbbBind(typeShift(d, ty), k)
    case c: TypeClassBind      => c
    case TypeClassInstanceBind(c, ty, m) =>
      TypeClassInstanceBind(c, typeShift(d, ty), m)
    case TermAbbBind(term, Some(ty)) =>
      TermAbbBind(term, Some(typeShift(d, ty)))
    case t @ TermAbbBind(_, None) => t
  }

  def termSubstituteType(
      tyS: Type,
      term: Term,
      idx: Integer
  ): Term =
    termMap(
      (info, c, k, n) =>
        if (k >= c) TermVar(info, k - 1, n - 1) else TermVar(info, k, n - 1),
      tyS,
      idx,
      term
    )

  def termMap(
      onTermVar: ShiftVarFunc[Term],
      tyS: Type,
      c: Int,
      t: Term
  ): Term = {
    def iter(c: Int, term: Term): Term = term match {
      case TermVar(info, x, n) => onTermVar(info, c, x, n)
      case TermAbs(info, i, ty, e, r) =>
        TermAbs(
          info,
          i,
          typeSubstitute(tyS, c, ty),
          iter(c + 1, e),
          r.map(typeSubstitute(tyS, c + 1, _))
        )
      case TermClosure(info, i, ty, e) =>
        TermClosure(
          info,
          i,
          ty.map(typeSubstitute(tyS, c, _)),
          iter(c + 1, e)
        )
      case TermApp(info, t1, t2) => TermApp(info, iter(c, t1), iter(c, t2))
      case TermFix(info, t)      => TermFix(info, iter(c, t))
      case TermMatch(info, t, cases) =>
        TermMatch(info, t, cases.map((p, e) => (p, iter(c, e))))
      case TermLet(info, i, t1, t2) =>
        TermLet(info, i, iter(c, t1), iter(c, t2))
      case TermProj(info, t, i)       => TermProj(info, iter(c, t), i)
      case TermMethodProj(info, t, i) => TermMethodProj(info, iter(c, t), i)
      case TermAssocProj(info, ty, i) =>
        TermAssocProj(info, typeSubstitute(tyS, c, ty), i)
      case TermRecord(info, v) =>
        TermRecord(info, v.map((i, t) => (i, iter(c, t))))
      case TermTag(info, i, t, ty) =>
        TermTag(info, i, iter(c, t), typeSubstitute(tyS, c, ty))
      case TermAscribe(info, t, ty) =>
        TermAscribe(info, iter(c, t), typeSubstitute(tyS, c, ty))
      case TermTApp(info, t, ty) => TermTApp(info, iter(c, t), ty)
      case v                     => v
    }
    iter(c, t)
  }

  def typeSubstituteTop(tyS: Type, tyT: Type): Type =
    typeShift(-1, typeSubstitute(typeShift(1, tyS), 0, tyT))

  def typeSubstitute(tyS: Type, c: Int, tyT: Type): Type =
    typeMap(
      (info, j, x, n) => if (x == j) typeShift(j, tyS) else TypeVar(info, x, n),
      c,
      tyT
    )

  def typeShift(d: Int, ty: Type): Type =
    typeShiftAbove(d, 0, ty)

  def typeShiftAbove(d: Int, c: Int, ty: Type): Type =
    typeMap(
      (info, c, k, n) =>
        if (k >= c) TypeVar(info, k + d, n + d) else TypeVar(info, k, n + d),
      c,
      ty
    )

  def typeMap(onVar: ShiftVarFunc[Type], c: Int, t: Type): Type = {
    def iter(c: Int, tyT: Type): Type = tyT match {
      case TypeVar(info, x, n) => onVar(info, c, x, n)
      case TypeEVar(_, _, _)   => tyT
      case TypeAny(_)          => tyT
      case TypeId(_, _)        => tyT
      case TypeClass(_, _)     => tyT
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
      case TypeAll(info, x, k, cls, tyTi) =>
        TypeAll(info, x, k, cls, iter(c + 1, tyTi))
      case TypeAbs(info, x, tyTi) => TypeAbs(info, x, iter(c + 1, tyTi))
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
