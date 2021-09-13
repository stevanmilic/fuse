package core

import cats.data.EitherT
import cats.data.State
import cats.implicits._
import parser.Identifiers._
import core.Context._

sealed trait TypeError

case class AscribeWrongTypeError(
    info: Info,
    termType: Type,
    ascribedType: Type,
    code: String = "E0001"
) extends TypeError

case class VariableNotFunctionTypeError(
    info: Info,
    termType: Type,
    code: String = "E0002"
) extends TypeError

case class MismatchFunctionTypeError(
    info: Info,
    termType: Type,
    expectedType: Type,
    code: String = "E0003"
) extends TypeError

case class TypeArgumentsNotAllowedTypeError(
    info: Info,
    termType: Type,
    code: String = "E0004"
) extends TypeError

case class InvalidTypeArgumentTypeError(
    info: Info,
    ty: Type,
    code: String = "E0005"
) extends TypeError

case class NoFieldsOnTypeError(
    info: Info,
    termType: Type,
    code: String = "E0006"
) extends TypeError

case class FieldNotFoundTypeError(
    info: Info,
    ty: Type,
    field: String,
    code: String = "E0007"
) extends TypeError

case class NoMethodsOnTypeError(
    info: Info,
    termType: Type,
    code: String = "E0008"
) extends TypeError

case class MethodNotFoundTypeError(
    info: Info,
    termType: Type,
    method: String,
    code: String = "E0009"
) extends TypeError

case class NotFoundTypeError(
    info: Info,
    variable: String = "",
    code: String = "E0010"
) extends TypeError

case class WrongReturnTypeError(
    info: Info,
    termType: Type,
    expectedType: Type,
    code: String = "E0011"
) extends TypeError

case class MatchTypeMismatchPatternTypeError(
    info: Info,
    matchExprType: Type,
    patternType: Type,
    code: String = "E0012"
) extends TypeError

case class MatchCasesTypeError(
    info: Info,
    caseExprType: Type,
    expectedType: Type,
    code: String = "E0013"
) extends TypeError

case class MatchExprNotDataTypeError(
    info: Info,
    exprType: Type,
    pattern: String,
    code: String = "E0014"
) extends TypeError

case class MatchVariantPatternMismatchTypeError(
    info: Info,
    variantType: Type,
    pattern: String,
    code: String = "E0015"
) extends TypeError

case class MatchRecordPatternMismatchTypeError(
    info: Info,
    variantType: Type,
    pattern: String,
    code: String = "E0016"
) extends TypeError

case class MatchPatternWrongVarsTypeError(
    info: Info,
    exprType: Type,
    expectedVariables: Int,
    patternVariables: Int,
    code: String = "E0017"
) extends TypeError

case class InvalidFunctionTypeError(
    info: Info,
    ty: Type,
    code: String = "E0018"
) extends TypeError

case class InvalidFoldForRecursiveTypeError(
    info: Info,
    ty: Type,
    code: String = "E0019"
) extends TypeError

case class TagNotVariantTypeError(
    info: Info,
    ty: Type,
    code: String = "E0020"
) extends TypeError

case class TagVariantFieldNotFoundTypeError(
    info: Info,
    variantType: Type,
    field: String,
    code: String = "E0021"
) extends TypeError

case class TagFieldMismatchTypeError(
    info: Info,
    termType: Type,
    expectedType: Type,
    code: String = "E0022"
) extends TypeError

case class NoTypArgumentsTypeError(
    info: Info,
    ty: Type,
    code: String = "E0023"
) extends TypeError

case class KindParameterMismatchTypeError(
    info: Info,
    ty: Type,
    typeKind: Kind,
    expectedKind: Kind,
    code: String = "E0024"
) extends TypeError

case class NoKindForTypeError(
    info: Info,
    typeIdx: Int,
    code: String = "E0025"
) extends TypeError

case class BindingNotFoundTypeError(
    info: Info,
    code: String = "E0026"
) extends TypeError

case class NoTypeForVariableTypeError(
    info: Info,
    varIdx: Int,
    code: String = "E0027"
) extends TypeError

case class WrongBindingForVariableTypeError(
    info: Info,
    varIdx: Int,
    code: String = "E0028"
) extends TypeError

object TypeErrorFormatter {
  def formatError[T](error: TypeError): StateEither[T] = {
    val errorMessage = error match {
      case AscribeWrongTypeError(info, termType, ascribedType, code) =>
        for {
          termTypeName <- Representation.typeToString(termType)
          ascribedTypeName <- Representation.typeToString(ascribedType)
        } yield errorWithCode(
          s"ascribed term is `$termTypeName`, expected `$ascribedTypeName`",
          code,
          info
        )
      case VariableNotFunctionTypeError(info, termType, code) =>
        Representation
          .typeToString(termType)
          .map(name =>
            errorWithCode(
              s"expected function, found `$name`",
              code,
              info
            )
          )
      case MismatchFunctionTypeError(info, termType, expectedType, code) =>
        for {
          termTypeName <- Representation.typeToString(termType)
          expectedTypeName <- Representation.typeToString(expectedType)
        } yield errorWithCode(
          s"expected `$expectedTypeName`, found `$termTypeName`",
          code,
          info
        )
      case TypeArgumentsNotAllowedTypeError(info, termType, code) =>
        Representation
          .typeToString(termType)
          .map(name =>
            errorWithCode(
              s"type arguments are not allowed for type `$name`",
              code,
              info
            )
          )
      case InvalidTypeArgumentTypeError(info, ty, code) =>
        for {
          typeName <- Representation.typeToString(ty)
        } yield errorWithCode(
          s"missing generics for type `$typeName`, expected type arguments",
          code,
          info
        )
      case NoFieldsOnTypeError(info, termType, code) =>
        Representation
          .typeToString(termType)
          .map(name =>
            errorWithCode(
              s"`$name` isn't a record type and therefore doesn't have fields",
              code,
              info
            )
          )
      case FieldNotFoundTypeError(info, termType, field, code) =>
        Representation
          .typeToString(termType)
          .map(name =>
            errorWithCode(
              s"no field `$field` on `$name` record",
              code,
              info
            )
          )
      case NoMethodsOnTypeError(info, termType, code) =>
        Representation
          .typeToString(termType)
          .map(name =>
            errorWithCode(
              s"`$name` isn't a data type and therefore can't have methods",
              code,
              info
            )
          )
      case MethodNotFoundTypeError(info, termType, method, code) =>
        Representation
          .typeToString(termType)
          .map(name =>
            errorWithCode(
              s"`$method` method not found in `$name` type",
              code,
              info
            )
          )
      case NotFoundTypeError(info, varName, code) =>
        errorWithCode(
          s"type not found $varName",
          code,
          info
        ).pure[StateEither]
      case WrongReturnTypeError(info, termType, expectedType, code) =>
        for {
          termTypeName <- Representation.typeToString(termType)
          expectedTypeName <- Representation.typeToString(expectedType)
        } yield errorWithCode(
          s"expected `$expectedTypeName` return type, found `$termTypeName`",
          code,
          info
        )
      case MatchTypeMismatchPatternTypeError(
            info,
            matchType,
            patternType,
            code
          ) =>
        for {
          matchTypeName <- Representation.typeToString(matchType)
          patternTypeName <- Representation.typeToString(patternType)
        } yield errorWithCode(
          s"expected `$matchTypeName` match expression type, found `$patternTypeName`",
          code,
          info
        )
      case MatchCasesTypeError(
            info,
            caseExprType,
            expectedType,
            code
          ) =>
        for {
          caseExprTypeName <- Representation.typeToString(caseExprType)
          expectedTypeName <- Representation.typeToString(expectedType)
        } yield errorWithCode(
          s"expected `$expectedTypeName` case expression type, found `$caseExprTypeName`",
          code,
          info
        )
      case MatchExprNotDataTypeError(
            info,
            matchExprType,
            pattern,
            code
          ) =>
        Representation
          .typeToString(matchExprType)
          .map(name =>
            errorWithCode(
              s"`$name` isn't a data type and therefore `$pattern` pattern can't be used",
              code,
              info
            )
          )
      case MatchVariantPatternMismatchTypeError(
            info,
            matchExprType,
            pattern,
            code
          ) =>
        Representation
          .typeToString(matchExprType)
          .map(name =>
            errorWithCode(
              s"variant `$name` type doesn't have `$pattern` option",
              code,
              info
            )
          )
      case MatchRecordPatternMismatchTypeError(
            info,
            matchExprType,
            pattern,
            code
          ) =>
        Representation
          .typeToString(matchExprType)
          .map(name =>
            errorWithCode(
              s"record `$name` type doesn't have `$pattern` field",
              code,
              info
            )
          )
      case MatchPatternWrongVarsTypeError(
            info,
            matchExprType,
            expectedVariables,
            patternVariables,
            code
          ) =>
        Representation
          .typeToString(matchExprType)
          .map(name =>
            errorWithCode(
              s"wrong number of variables for `$name`, expected $expectedVariables, given $patternVariables",
              code,
              info
            )
          )
      case InvalidFunctionTypeError(
            info,
            ty,
            code
          ) =>
        Representation
          .typeToString(ty)
          .map(name =>
            errorWithCode(
              s"expected a function type, found `$name`",
              code,
              info
            )
          )
      case InvalidFoldForRecursiveTypeError(
            info,
            ty,
            code
          ) =>
        Representation
          .typeToString(ty)
          .map(name =>
            errorWithCode(
              s"expected a recursive type, found `$name`",
              code,
              info
            )
          )
      case NoTypArgumentsTypeError(info, ty, code) =>
        Representation
          .typeToString(ty)
          .map(name =>
            errorWithCode(
              s"type arguments are not allowed for `$name` type",
              code,
              info
            )
          )
      case KindParameterMismatchTypeError(
            info,
            ty,
            typeKind,
            expectedKind,
            code
          ) =>
        val typeKindRepr = Representation.kindToString(typeKind)
        val expectedKindRepr = Representation.kindToString(expectedKind)
        Representation
          .typeToString(ty)
          .map(name =>
            errorWithCode(
              s"expected `$expectedKindRepr` kind, given `$typeKindRepr` with `$name` type",
              code,
              info
            )
          )
      case TagNotVariantTypeError(info, ty, code) =>
        Representation
          .typeToString(ty)
          .map(name =>
            errorWithCode(
              s"expected variant type, given `$name`",
              code,
              info
            )
          )
      case TagVariantFieldNotFoundTypeError(info, ty, field, code) =>
        Representation
          .typeToString(ty)
          .map(name =>
            errorWithCode(
              s"variant `$name` type doesn't have `$field` field",
              code,
              info
            )
          )
      case TagFieldMismatchTypeError(info, termType, expectedType, code) =>
        for {
          termTypeName <- Representation.typeToString(termType)
          expectedTypeName <- Representation.typeToString(expectedType)
        } yield errorWithCode(
          s"expected `$expectedTypeName` variant type, found `$termTypeName`",
          code,
          info
        )
      case BindingNotFoundTypeError(info, code) =>
        errorWithCode("variable not found", code, info).pure[StateEither]
      case NoKindForTypeError(info, _, code) =>
        errorWithCode("no kind recorded for type", code, info).pure[StateEither]
      case NoTypeForVariableTypeError(info, idx, code) =>
        EitherT.liftF(
          State.inspect((ctx: Context) =>
            errorWithCode(
              s"no type recorded for variable `${indexToName(ctx, idx)}`",
              code,
              info
            )
          )
        )
      case WrongBindingForVariableTypeError(info, idx, code) =>
        EitherT.liftF(
          State.inspect((ctx: Context) =>
            errorWithCode(
              s"wrong kind of variable for `${indexToName(ctx, idx)}`",
              code,
              info
            )
          )
        )
    }
    errorMessage.flatMap(e => EitherT.leftT[ContextState, T](e))
  }

  def errorWithCode(message: String, code: String, info: Info) = {
    val errorCode = fansi.Bold.On(fansi.Color.LightRed(f"error[$code]"))
    val messageBold = fansi.Bold.On(f": $message")
    f"$errorCode$messageBold --> ${info.show}"
  }
}
