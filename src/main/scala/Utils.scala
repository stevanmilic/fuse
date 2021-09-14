package fuse

import parser.Identifiers._
import cats.implicits._

object Utils {
  def consoleError(message: String, info: Info, code: Option[String] = None) = {
    val errorRepr = code match {
      case Some(code) => s"error[$code]"
      case None       => "error"
    }
    val errorBold = fansi.Bold.On(fansi.Color.LightRed(errorRepr))
    val messageBold = fansi.Bold.On(s": $message")
    s"$errorBold$messageBold${info.show}"
  }
}
