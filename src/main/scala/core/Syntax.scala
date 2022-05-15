package core

import parser.Info.*

object Types {
  sealed trait Kind
  case object KindStar extends Kind
  case class KindArrow(k1: Kind, k2: Kind) extends Kind

  sealed trait Type

  case class TypeVar(info: Info, index: Integer, length: Integer) extends Type
  case class TypeId(info: Info, i: String) extends Type
  case class TypeArrow(info: Info, t1: Type, t2: Type) extends Type
  case class TypeUnit(info: Info) extends Type
  case class TypeRecord(info: Info, f: List[(String, Type)]) extends Type
  case class TypeVariant(info: Info, v: List[(String, Type)]) extends Type
  case class TypeRec(info: Info, v: String, k: Kind, t: Type) extends Type
  case class TypeAll(info: Info, i: String, k: Kind, t: Type) extends Type
  case class TypeAbs(info: Info, i: String, t: Type) extends Type
  case class TypeApp(info: Info, t1: Type, t2: Type) extends Type
  case class TypeBool(info: Info) extends Type
  case class TypeString(info: Info) extends Type
  case class TypeFloat(info: Info) extends Type
  case class TypeInt(info: Info) extends Type

  implicit val showTypeInfo: ShowInfo[Type] = ShowInfo.info(_ match {
    case TypeVar(info, _, _)    => info
    case TypeId(info, _)        => info
    case TypeArrow(info, _, _)  => info
    case TypeUnit(info)         => info
    case TypeRecord(info, _)    => info
    case TypeVariant(info, _)   => info
    case TypeRec(info, _, _, _) => info
    case TypeAll(info, _, _, _) => info
    case TypeAbs(info, _, _)    => info
    case TypeApp(info, _, _)    => info
    case TypeBool(info)         => info
    case TypeString(info)       => info
    case TypeFloat(info)        => info
    case TypeInt(info)          => info
  })
}

object Terms {
  import Types.*

  sealed trait Term

  // Abstraction
  case class TermFix(info: Info, t: Term) extends Term
  case class TermAbs(
      info: Info,
      i: String,
      t: Type,
      e: Term,
      r: Option[Type] = None
  ) extends Term
  case class TermClosure(info: Info, i: String, t: Option[Type], e: Term)
      extends Term
  // Expressions
  case class TermVar(info: Info, i1: Integer, i2: Integer) extends Term
  case class TermApp(info: Info, f: Term, v: Term) extends Term
  case class TermMatch(info: Info, t: Term, c: List[(Pattern, Term)])
      extends Term
  case class TermLet(info: Info, i: String, t1: Term, t2: Term) extends Term
  case class TermProj(info: Info, t: Term, i: String) extends Term
  case class TermMethodProj(info: Info, t: Term, i: String) extends Term
  // ADT
  case class TermRecord(info: Info, v: List[(String, Term)]) extends Term
  case class TermTag(info: Info, i: String, t: Term, typ: Type) extends Term
  case class TermAscribe(info: Info, t: Term, typ: Type) extends Term
  case class TermFold(info: Info, t: Type) extends Term
  // Higher Kind
  case class TermTAbs(info: Info, i: String, t: Term) extends Term
  case class TermTApp(info: Info, t: Term, typ: Type) extends Term
  // Built-in functions with pre-defined type
  case class TermBuiltin(typ: Type) extends Term

  sealed trait Pattern

  // Literals + Patterns
  case class TermFloat(info: Info, f: Float) extends Term with Pattern
  case class TermInt(info: Info, i: Int) extends Term with Pattern
  case class TermString(info: Info, s: String) extends Term with Pattern
  case class TermTrue(info: Info) extends Term with Pattern
  case class TermFalse(info: Info) extends Term with Pattern
  case class TermUnit(info: Info) extends Term with Pattern

  case class PatternNode(info: Info, tag: String, vars: List[String] = List())
      extends Pattern
  case class PatternDefault(info: Info) extends Pattern

  implicit val showTermInfo: ShowInfo[Term] = ShowInfo.info(_ match {
    case TermFloat(info, _)         => info
    case TermInt(info, _)           => info
    case TermString(info, _)        => info
    case TermTrue(info)             => info
    case TermFalse(info)            => info
    case TermUnit(info)             => info
    case TermBuiltin(_)             => UnknownInfo
    case TermFold(info, _)          => info
    case TermTApp(info, _, _)       => info
    case TermTAbs(info, _, _)       => info
    case TermAscribe(info, _, _)    => info
    case TermTag(info, _, _, _)     => info
    case TermRecord(info, _)        => info
    case TermMethodProj(info, _, _) => info
    case TermFix(info, _)           => info
    case TermAbs(info, _, _, _, _)  => info
    case TermClosure(info, _, _, _) => info
    case TermVar(info, _, _)        => info
    case TermApp(info, _, _)        => info
    case TermMatch(info, _, _)      => info
    case TermLet(info, _, _, _)     => info
    case TermProj(info, _, _)       => info
  })

  implicit val showPatternInfo: ShowInfo[Pattern] = ShowInfo.info(_ match {
    case TermFloat(info, _)      => info
    case TermInt(info, _)        => info
    case TermString(info, _)     => info
    case TermTrue(info)          => info
    case TermFalse(info)         => info
    case TermUnit(info)          => info
    case PatternNode(info, _, _) => info
    case PatternDefault(info)    => info
  })
}

object Bindings {
  import Types.*
  import Terms.*

  sealed trait Binding

  case object NameBind extends Binding
  case class TypeVarBind(k: Kind) extends Binding
  case class VarBind(t: Type) extends Binding
  case class TypeAbbBind(t: Type, k: Option[Kind] = None) extends Binding
  case class TermAbbBind(t: Term, ty: Option[Type] = None) extends Binding
  // Used by code generation phase (grin).
  case object TempVarBind extends Binding

// Global bindings.
  case class Bind(i: String, b: Binding)
}
