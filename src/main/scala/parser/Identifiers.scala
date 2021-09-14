package parser

import cats.Show
import org.parboiled2._

import scala.language.implicitConversions

object Identifiers {
  sealed trait FToken

  case class FIdentifier(value: String) extends FToken
  case class FIndent(size: Int) extends FToken
  case class Location(line: Int, column: Int, lineInput: String)

  sealed trait Info
  case class FileInfo(
      location: Location,
      fileName: String
  ) extends Info
  case object UnknownInfo extends Info

  implicit val showInfo: Show[Info] =
    Show.show(info =>
      info match {
        case FileInfo(location, fileName) =>
          val padding = List.fill(location.line.toString().length)(" ").mkString
          val errorSign = fansi.Bold.On(fansi.Color.LightRed("^"))
          val errorIndicator =
            List.fill(location.column - 1)(" ").mkString + errorSign
          List(
            s"\n$padding--> ${fileName}:${location.line}:${location.column}",
            fansi.Bold.On(s"$padding |").toString(),
            fansi.Bold
              .On(s"${location.line} |")
              .toString() + s" ${location.lineInput}",
            fansi.Bold.On(s"$padding | ").toString() + errorIndicator
          ).mkString("\n")
        case UnknownInfo => ""
      }
    )

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

abstract class Identifiers extends Keywords {
  import CharPredicate.{AlphaNum}
  import Identifiers._

  implicit def wspStrR(s: String): Rule0 = rule {
    str(s) ~ zeroOrMore(' ')
  }

  def wspStr(s: String): Rule0 = rule {
    zeroOrMore(' ') ~ wspStrR(s)
  }

  def loc: Location = {
    val pos = Position(cursor, input)
    Location(pos.line, pos.column, input.getLine(pos.line))
  }

  def mark = rule { atomic("") ~> (() => loc) }

  def Id = rule { capture(IdentifierPart) ~> FIdentifier }
  def IdentifierPart = rule { AlphaNum_.+ }
  def NewLine = rule(quiet('\r'.? ~ '\n'))
  def Indent = rule { capture(Spacing.+) ~> (s => FIndent(s.size)) }
  def Spacing = rule { ch('\t') | ch(' ') }
  def WL = rule(quiet((Spacing | NewLine).*))

  // Meta rule that matches one or more indented lines with the specified
  // rule. Accepts a `Function0` argument to prevent expansion of the passed
  // rule.  It's best to pass the a `val` member to the function in order to
  // prevent the re-allocation during every execution of the meta rule.
  // More info: https://github.com/sirthias/parboiled2#meta-rules
  def oneOrMoreWithIndent[T](r: () => Rule1[T]): Rule1[Seq[T]] = rule {
    (NewLine.+ ~ Indent ~ r() ~> ((_, _))).+ ~> ((lines: Seq[(FIndent, T)]) => {
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
