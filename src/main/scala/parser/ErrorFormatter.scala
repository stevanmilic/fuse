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
}
