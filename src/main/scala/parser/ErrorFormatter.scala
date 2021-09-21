package parser

import org.parboiled2._
import java.lang.{StringBuilder => JStringBuilder}
import fuse.Utils.consoleError
import Info._

class ParserErrorFormatter(fileName: String) extends ErrorFormatter {

  override def format(
      error: ParseError,
      input: ParserInput
  ): String = {
    val problem = formatProblem(error, input)
    val expected = formatExpected(error)
    import error._
    consoleError(
      s"${problem.charAt(0).toLower.toString + problem.substring(1)}$expected",
      FileInfo(
        Location(position.line, position.column, input.getLine(position.line)),
        fileName
      )
    )
  }

  override def formatNonTerminal(
      nonTerminal: RuleTrace.NonTerminal,
      showFrameStartOffset: Boolean = true
  ): String =
    camelCaseToWords(super.formatNonTerminal(nonTerminal, showFrameStartOffset))

  override def formatTerminal(terminal: RuleTrace.Terminal): String =
    camelCaseToWords(super.formatTerminal(terminal))

  private def camelCaseToWords(name: String) = name
    .replaceAll(
      "([a-z])([A-Z]+)",
      "$1 $2"
    )
    .toLowerCase()
}
