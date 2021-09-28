package core

import core.Bindings._
import core.Terms._
import core.Types._
import parser.Info._

object BuiltIn {
  // TODO: Add built-ins for the rest of the prim ops.

  val Functions = List(
    Bind(
      "&add",
      TermAbbBind(
        TermBuiltin(
          TypeArrow(
            UnknownInfo,
            TypeInt(UnknownInfo),
            TypeArrow(UnknownInfo, TypeInt(UnknownInfo), TypeInt(UnknownInfo))
          )
        )
      )
    ),
    Bind(
      "&sub",
      TermAbbBind(
        TermBuiltin(
          TypeArrow(
            UnknownInfo,
            TypeInt(UnknownInfo),
            TypeArrow(UnknownInfo, TypeInt(UnknownInfo), TypeInt(UnknownInfo))
          )
        )
      )
    ),
    Bind(
      "&multiply",
      TermAbbBind(
        TermBuiltin(
          TypeArrow(
            UnknownInfo,
            TypeInt(UnknownInfo),
            TypeArrow(UnknownInfo, TypeInt(UnknownInfo), TypeInt(UnknownInfo))
          )
        )
      )
    ),
    Bind(
      "print",
      TermAbbBind(
        TermBuiltin(
          TypeArrow(
            UnknownInfo,
            TypeString(UnknownInfo),
            TypeUnit(UnknownInfo),
          )
        )
      )
    ),
    Bind(
      "int_to_str",
      TermAbbBind(
        TermBuiltin(
          TypeArrow(
            UnknownInfo,
            TypeInt(UnknownInfo),
            TypeString(UnknownInfo),
          )
        )
      )
    )
  )
}
