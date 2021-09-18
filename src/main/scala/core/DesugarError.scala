package core

import cats.data.EitherT
import cats.data.State
import cats.implicits._
import core.Context._
import fuse.Utils.consoleError
import parser.Info._


sealed trait DesugarError

case class TypeNotSupportedDesugarError(info: Info) extends DesugarError
case class ExpressionNotSupportedDesugarError(info: Info) extends DesugarError
case class DeclarationNotSupportedDesugarError(info: Info) extends DesugarError
case class VariableNotFoundDesugarError(info: Info, variable: String)
    extends DesugarError
case class TypeVariableNotFoundDesugarError(info: Info, variable: String)
    extends DesugarError
case class ExpressionNotValidDesugarError(info: Info) extends DesugarError
case class FunctionOperatorNotFoundDesugarError(info: Info, func: String)
    extends DesugarError
case class CaseNotSupportedDesugarError(info: Info) extends DesugarError
case class NestedPatternNotSupportedDesugarError(info: Info)
    extends DesugarError

object DesugarError {
  def format[T](error: DesugarError): StateEither[T] =
    EitherT.leftT(error match {
      case TypeNotSupportedDesugarError(info) =>
        consoleError("type not supported", info)
      case ExpressionNotSupportedDesugarError(info) =>
        consoleError("expression not supported", info)
      case DeclarationNotSupportedDesugarError(info) =>
        consoleError("declaration not supported", info)
      case VariableNotFoundDesugarError(info, variable) =>
        consoleError(s"cannot find variable `$variable` in this scope", info)
      case TypeVariableNotFoundDesugarError(info, variable) =>
        consoleError(s"cannot find type variable `$variable` in this scope", info)
      case FunctionOperatorNotFoundDesugarError(info, func) =>
        consoleError(s"function operator `$func` not found", info)
      case ExpressionNotValidDesugarError(info) =>
        consoleError(
          s"expression value not used, assign the value with `let` binding",
          info
        )
      case CaseNotSupportedDesugarError(info) =>
        consoleError("case not supported", info)
      case NestedPatternNotSupportedDesugarError(info) =>
        consoleError("nested pattern not supported", info)
    })
}
