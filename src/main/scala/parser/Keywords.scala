package parser

import org.parboiled2._

import scala.annotation.switch

abstract class Keywords extends Parser {
  import CharPredicate.{AlphaNum}

  def Keyword = rule(AlphaKeyword)

  val Underscore = CharPredicate('_')

  val AlphaNum_ = AlphaNum ++ Underscore

  private def AlphaKeyword = rule {
    run {
      (cursorChar: @switch) match {
        case 'f' => "fun" | "false"
        case 'i' => "import" | "impl"
        case 'm' => str("match")
        case 'l' => str("let")
        case 't' => "true" | "type"
        case '_' => ANY
        case _   => MISMATCH
      }
    } ~ !AlphaNum_
  }
}
