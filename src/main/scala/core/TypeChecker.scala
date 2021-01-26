package core

import core.Context._
import parser.FuseParser._
import scala.util._

object TypeChecker {

  def typeCheck(bindings: List[Binding], c: Context): List[String] =
    bindings
      .foldLeft((c, List[String]()))((acc, b) =>
        evalBinding(acc._1, b) match {
          case Right(c) => (c, acc._2)
          case Left(e)  => (c, e :: acc._2)
        }
      )
      ._2

  def evalBinding(c: Context, b: Binding): Either[String, Context] =
    ???

  def typeOf(c: Context, t: Term): Type =
    ???
}
