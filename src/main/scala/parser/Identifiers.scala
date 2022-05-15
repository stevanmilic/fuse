package parser

import org.parboiled2.*

import scala.language.implicitConversions
import scala.util.Try

object Identifiers {
  sealed trait FToken

  case class FIdentifier(value: String) extends FToken
  case class FIndent(size: Int) extends FToken

  def checkIndents(
      indents: Seq[FIndent],
      parentIndent: Option[FIndent],
      equalToParent: Boolean
  ): Boolean = {
    val firstIndentSize = indents.head.size
    val sameSize = (indent: FIndent) => indent.size == firstIndentSize
    parentIndent match {
      case Some(FIndent(parentSize)) =>
        (firstIndentSize > parentSize || (equalToParent && firstIndentSize == parentSize)) && indents
          .forall(sameSize)
      case None => indents.forall(sameSize)
    }
  }

  def findParentIndent(elems: List[Any]): Option[FIndent] = elems.collectFirst {
    case (i: FIndent) => i
  }
}

abstract class Identifiers(fileName: String) extends Keywords {
  import CharPredicate.{AlphaNum}
  import Identifiers.*
  import Info.*

  implicit def any(s: String): Rule0 = rule {
    str(s) ~ WS
  }

  def loc: Location = {
    val Position(_, line, column) = Position(cursor, input)
    Location(line, column, input.getLine(line))
  }

  def info = rule { atomic("") ~> (() => FileInfo(loc, fileName)) }

  def identifier = rule {
    quiet(Spacing.*) ~ capture(IdentifierPart) ~> FIdentifier.apply
  }
  def IdentifierPart = rule { AlphaNum_.named("alphanumeric").+ }
  def NewLine = rule(quiet('\r'.? ~ '\n'))
  def Indent = rule { quiet(capture(Spacing.+)) ~> (s => FIndent(s.size)) }
  def WS = rule(quiet((Spacing | Comment).*))
  def WL = rule(quiet((Spacing | NewLine).*))

  def Comment = rule { '#' ~ (!NewLine ~ ANY).* }
  def CommentLine = rule(quiet(Spacing.* ~ Comment ~ Spacing.* ~ NewLine))
  def LineEnd = rule { Spacing.* ~ NewLine }

  def EmptyLine = rule { LineEnd | CommentLine }

  // Meta rule that matches one or more indented lines with the specified
  // rule. Accepts a `Function0` argument to prevent expansion of the passed
  // rule.  It's best to pass the a `val` member to the function in order to
  // prevent the re-allocation during every execution of the meta rule.
  // More info: https://github.com/sirthias/parboiled2#meta-rules
  def oneOrMoreWithIndent[T](
      r: () => Rule1[T],
      ruleName: String
  ): Rule1[Seq[T]] = rule {
    (EmptyLine.+ ~ Indent ~ r()
      .named(ruleName) ~> ((_, _))).+ ~> ((lines: Seq[(FIndent, T)]) => {
      val (indents, nodes) = lines.unzip
      validateIndents(indents) ~ push(nodes) | failX(
        "correctly indented block"
      )
    })
  }

  // Checks if the indents are correctly aligned in respect to any indentation
  // in the parent block.
  def validateIndents(
      indents: Seq[FIndent],
      equalToParent: Boolean = false
  ): Rule0 =
    rule {
      test(
        checkIndents(
          indents,
          findParentIndent(valueStack.toList.reverse),
          equalToParent
        )
      )
    }
}
