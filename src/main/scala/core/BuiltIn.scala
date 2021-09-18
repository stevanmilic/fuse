package core

import core.Bindings._
import core.Terms._
import core.Types._

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
