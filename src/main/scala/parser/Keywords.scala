package parser

import org.parboiled2.*

import scala.annotation.switch

abstract class Keywords extends Parser {
  import CharPredicate.{AlphaNum}

  def `fun` = Key("fun")
  def `import` = Key("import")
  def `impl` = Key("impl")
  def `match` = Key("match")
  def `let` = Key("let")
  def `type` = Key("type")
  def `true` = Key("true")
  def `trait` = Key("trait")
  def `false` = Key("false")
  def `if` = Key("false")
  def `Unit` = Key("Unit")

  def `for` = symbol("for")
  def `:` = symbol(':')
  def `=` = symbol('=')
  def `=>` = symbol("=>")
  def `->` = symbol("->")
  def `@` = symbol("->")
  def `*` = symbol("*")
  def `/` = symbol("/")
  def `%` = symbol("%")
  def `+ ` = symbol("+")
  def `- ` = symbol("-")
  def `<=` = symbol("<=")
  def `>=` = symbol(">=")
  def `<` = symbol("<")
  def `>` = symbol(">")
  def `==` = symbol("==")
  def `!=` = symbol("!=")
  def `&&` = symbol("&&")
  def `||` = symbol("||")

  def Keyword = rule(AlphaKeyword)

  def Spacing = rule { ch('\t') | ch(' ') }
  val Underscore = CharPredicate('_')
  val AlphaNum_ = AlphaNum ++ Underscore

  private def Key(s: String) = rule { s ~ !AlphaNum_ }
  private def symbol(s: Char) = rule(
    quiet(Spacing.*) ~ s ~ quiet(Spacing.*)
  )
  private def symbol(s: String) = rule(quiet(Spacing.*) ~ s ~ quiet(Spacing.*))

  private def AlphaKeyword = rule {
    run {
      (cursorChar: @switch) match {
        case 'f' => "fun" | "false" | "for"
        case 'i' => "import" | "impl"
        case 'm' => str("match")
        case 'l' => str("let")
        case 't' => "true" | "type" | "trait"
        case 'U' => str("Unit")
        case '_' => ANY
        case _   => MISMATCH
      }
    } ~ !AlphaNum_
  }
}
