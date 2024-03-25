package core

import core.Bindings.*
import core.Terms.*
import core.Types.*
import parser.Info.*

object BuiltIn {
  def buildFunc(args: List[Type], r: Type): Type =
    (args :+ r).reduceRight(TypeArrow(UnknownInfo, _, _))

  def buildBind(name: String, binding: Binding): List[Bind] =
    List(Bind(name, binding))

  def buildClassMethodBind(name: String, ty: Type, cls: TypeClass): List[Bind] =
    buildBind(name, TermAbbBind(TermClassMethod(i, ty, cls)))

  def buildClassBind(name: String): List[Bind] =
    buildBind(name, TypeClassBind(KindStar))

  def buildClassInstanceBind(
      typeClass: String,
      typeName: String,
      ty: Type,
      method: String,
      body: Term
  ): List[Bind] =
    val typeClassInstance = buildBind(
      Desugar.toTypeInstanceBindID(typeName, typeClass),
      TypeClassInstanceBind(typeClass, ty, List(method))
    )
    val methodBind = buildBind(
      Desugar.toTypeInstanceMethodID(method, typeName, typeClass),
      TermAbbBind(body)
    )
    typeClassInstance ++ methodBind

  val i = UnknownInfo

  // TODO: Add built-ins for the rest of the prim ops.
  val Binds: List[Bind] = List(
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
      ),
      TypeClass(i, "Add")
    ),
    buildClassInstanceBind(
      "Add",
      "i32",
      TypeInt(i),
      "+",
      TermBuiltin(buildFunc(List(TypeInt(i), TypeInt(i)), TypeInt(i)))
    ),
    buildClassInstanceBind(
      "Add",
      "f32",
      TypeFloat(i),
      "+",
      TermBuiltin(buildFunc(List(TypeFloat(i), TypeFloat(i)), TypeFloat(i)))
    ),
    buildClassInstanceBind(
      "Add",
      "str",
      TypeString(i),
      "+",
      TermBuiltin(
        buildFunc(List(TypeString(i), TypeString(i)), TypeString(i))
      )
    ),
    // TODO: Use generic versions of operators.
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
  ).flatten
}
