package parser

import org.parboiled2._

object FuseLexicalParser {
  sealed trait FToken

  case class FIdentifier(value: String) extends FToken
  case class FIndent(size: Int) extends FToken

  def checkIndents(
      indents: Seq[FIndent],
      parentIndent: Option[FIndent]
  ): Boolean = {
    val firstIndentSize = indents.head.size
    val sameSize = (indent: FIndent) => indent.size == firstIndentSize
    parentIndent match {
      case Some(FIndent(parentSize)) =>
        firstIndentSize > parentSize && indents.forall(sameSize)
      case None => indents.forall(sameSize)
    }
  }
}

abstract class FuseLexicalParser extends Parser {
  import CharPredicate.{AlphaNum}
  import FuseLexicalParser._

  implicit def wspStrR(s: String): Rule0 = rule {
    str(s) ~ zeroOrMore(' ')
  }

  def wspStr(s: String): Rule0 = rule {
    zeroOrMore(' ') ~ wspStrR(s)
  }

  def Id = rule { capture(IdentifierPart) ~> FIdentifier }
  def IdentifierPart = rule { (AlphaNum | '_').+ }
  def NewLine = rule { ch('\n') }
  def Indent = rule { capture(Spacing.+) ~> (s => FIndent(s.size)) }
  def Spacing = rule { ch('\t') | ch(' ') }

  // Meta rule that matches one or more indentent lines with the specified
  // rule. Accepts a `Function0` argument to prevent expansion of the passed
  // rule.  It's best to pass the a `val` member to the function in order to
  // prevent the re-allocation during every execution of the meta rule.
  // More info: https://github.com/sirthias/parboiled2#meta-rules
  def oneOrMoreWithIndent[T](r: () => Rule1[T]): Rule1[Seq[T]] = rule {
    (NewLine ~ Indent ~ r() ~> ((_, _))).+ ~> ((lines: Seq[(FIndent, T)]) => {
      val (indents, nodes) = lines.unzip
      validateIndents(indents) ~ push(nodes) | failX(
        "correctly indendent block"
      )
    })
  }

  // Checks if the indents are correctly aligned in respect to any indentation
  // in the parent block.
  def validateIndents(indents: Seq[FIndent]): Rule0 = rule {
    test(
      checkIndents(
        indents,
        valueStack.toStream.reverse.collectFirst { case i @ FIndent(_) => i }
      )
    )
  }
}
