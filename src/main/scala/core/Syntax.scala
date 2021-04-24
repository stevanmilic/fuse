package core

sealed trait Type

case class TypeVar(i1: Integer, i2: Integer) extends Type
case class TypeId(i: String) extends Type
case class TypeArrow(t1: Type, t2: Type) extends Type
case object TypeUnit extends Type
case class TypeRecord(f: List[(String, Type)]) extends Type
case class TypeVariant(v: List[(String, Type)]) extends Type
case object TypeBool extends Type
case object TypeString extends Type
case object TypeFloat extends Type
case object TypeInt extends Type
case class TypeRec(v: String, t: Type) extends Type

sealed trait Term

case class TermCase(i: String, v: String, e: Term)
case class TermMatch(t: Term, c: List[TermCase]) extends Term
case class TermAbs(i: String, t: Type, e: Term) extends Term
case class TermVar(i1: Integer, i2: Integer) extends Term
case class TermApp(f: Term, v: Term) extends Term
case class TermLet(i: String, t1: Term, t2: Term) extends Term
case class TermRecord(v: List[(String, Term)]) extends Term
case class TermProj(t: Term, i: String) extends Term
case class TermFloat(f: Float) extends Term
case class TermInt(i: Int) extends Term
case class TermString(s: String) extends Term
case class TermFold(t: Term) extends Term
case class TermUnfold(t: Term) extends Term
case object TermTrue extends Term
case object TermFalse extends Term
case object TermUnit extends Term

sealed trait Binding

case object NameBind extends Binding
case object TypeVarBind extends Binding
case class VarBind(t: Type) extends Binding
case class TypeAbbBind(t: Type) extends Binding

// Global bindings.
case class Bind(i: String, b: Binding)
