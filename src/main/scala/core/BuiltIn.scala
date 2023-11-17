package core

import core.Bindings.*
import core.Terms.*
import core.Types.*
import parser.Info.*

object BuiltIn {
  def buildFunc(args: List[Type], r: Type): Type =
    (args :+ r).reduceRight(TypeArrow(UnknownInfo, _, _))

  def buildBind(name: String, binding: Binding): Bind =
    Bind(name, binding)

  def buildClassMethodBind(name: String, ty: Type): Bind =
    buildBind(name, TermAbbBind(TermClassMethod(i, ty)))

  def buildClassBind(name: String): Bind =
    buildBind(name, TypeClassBind(KindStar))

  def buildClassInstanceBind(
      name: String,
      typeClass: String,
      ty: Type,
      method: Tuple2[String, Term]
  ): Bind =
    buildBind(name, TypeClassInstanceBind(typeClass, ty, List(method._1)))

  val i = UnknownInfo

  // TODO: Add built-ins for the rest of the prim ops.
  val Binds = List(
    buildClassBind("Add"),
    buildClassMethodBind(
      "+",
      TypeAll(
        i,
        "T",
        KindStar,
        List(TypeClass(i, "Add")),
        buildFunc(
          List(TypeVar(i, 0, 1), TypeVar(i, 0, 1)),
          TypeVar(i, 0, 1)
        )
      )
    ),
    buildClassInstanceBind(
      "#Add#int",
      "Add",
      TypeInt(i),
      ("+", TermBuiltin(buildFunc(List(TypeInt(i), TypeInt(i)), TypeInt(i))))
    ),
    buildClassInstanceBind(
      "#Add#float",
      "Add",
      TypeFloat(i),
      (
        "+",
        TermBuiltin(buildFunc(List(TypeFloat(i), TypeFloat(i)), TypeFloat(i)))
      )
    ),
    buildClassInstanceBind(
      "#Add#string",
      "Add",
      TypeString(i),
      (
        "+",
        TermBuiltin(
          buildFunc(List(TypeString(i), TypeString(i)), TypeString(i))
        )
      )
    ),
    buildBind(
      "&sub",
      TermAbbBind(
        TermBuiltin(buildFunc(List(TypeInt(i), TypeInt(i)), TypeInt(i)))
      )
    ),
    buildBind(
      "&multiply",
      TermAbbBind(
        TermBuiltin(buildFunc(List(TypeInt(i), TypeInt(i)), TypeInt(i)))
      )
    ),
    buildBind(
      "&eq",
      TermAbbBind(
        TermBuiltin(buildFunc(List(TypeInt(i), TypeInt(i)), TypeInt(i)))
      )
    ),
    buildBind(
      "print",
      TermAbbBind(TermBuiltin(buildFunc(List(TypeString(i)), TypeUnit(i))))
    ),
    buildBind(
      "int_to_str",
      TermAbbBind(TermBuiltin(buildFunc(List(TypeInt(i)), TypeString(i))))
    )
  )
}
