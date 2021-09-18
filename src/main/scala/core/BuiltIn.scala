package core

import core.Bindings._
import core.Terms._
import core.Types._
import parser.Info._

object BuiltIn {
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
    )
  )
}
