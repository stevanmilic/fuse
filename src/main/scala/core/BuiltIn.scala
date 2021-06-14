package core

object BuiltIn {
  val Functions = List(
    Bind(
      "&add",
      TermAbbBind(
        TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
      )
    ),
    Bind(
      "&sub",
      TermAbbBind(
        TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
      )
    ),
    Bind(
      "&multiply",
      TermAbbBind(
        TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
      )
    )
  )
}
