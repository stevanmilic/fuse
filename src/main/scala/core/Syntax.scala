package core

sealed trait Kind
case object KindStar extends Kind
case class KindArrow(k1: Kind, k2: Kind) extends Kind

sealed trait Type

case class TypeVar(index: Integer, length: Integer) extends Type
case class TypeId(i: String) extends Type
case class TypeArrow(t1: Type, t2: Type) extends Type
case object TypeUnit extends Type
case class TypeRecord(f: List[(String, Type)]) extends Type
case class TypeVariant(v: List[(String, Type)]) extends Type
case class TypeRec(v: String, k: Kind, t: Type) extends Type
case class TypeAll(i: String, k: Kind, t: Type) extends Type
case class TypeAbs(i: String, t: Type) extends Type
case class TypeApp(t1: Type, t2: Type) extends Type
case object TypeBool extends Type
case object TypeString extends Type
case object TypeFloat extends Type
case object TypeInt extends Type

sealed trait Term

// Abstraction
case class TermFix(t: Term) extends Term
case class TermAbs(i: String, t: Type, e: Term, r: Option[Type] = None)
    extends Term
case class TermClosure(i: String, t: Option[Type], e: Term) extends Term
// Expressions
case class TermVar(i1: Integer, i2: Integer) extends Term
case class TermApp(f: Term, v: Term) extends Term
case class TermMatch(t: Term, c: List[(Pattern, Term)]) extends Term
case class TermLet(i: String, t1: Term, t2: Term) extends Term
case class TermProj(t: Term, i: String) extends Term
// ADT
case class TermRecord(v: List[(String, Term)]) extends Term
case class TermTag(i: String, t: Term, typ: Type) extends Term
case class TermAscribe(t: Term, typ: Type) extends Term
case class TermFold(t: Type) extends Term
case class TermUnfold(t: Type) extends Term
// Higher Kind
case class TermTAbs(i: String, t: Term) extends Term
case class TermTApp(t: Term, typ: Type) extends Term

sealed trait Pattern

// Literals + Patterns
case class TermFloat(f: Float) extends Term with Pattern
case class TermInt(i: Int) extends Term with Pattern
case class TermString(s: String) extends Term with Pattern
case object TermTrue extends Term with Pattern
case object TermFalse extends Term with Pattern
case object TermUnit extends Term with Pattern

case class PatternNode(tag: String, vars: List[String] = List()) extends Pattern
case object PatternDefault extends Pattern

sealed trait Binding

case object NameBind extends Binding
case class TypeVarBind(k: Kind) extends Binding
case class VarBind(t: Type) extends Binding
case class TypeAbbBind(t: Type, k: Option[Kind] = None) extends Binding
case class TermAbbBind(t: Term) extends Binding

// Global bindings.
case class Bind(i: String, b: Binding)
