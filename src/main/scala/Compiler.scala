package fuse

import cats.data.EitherT
import cats.data.State
import cats.effect.IO
import cats.implicits._
import code.Grin
import core.Bindings._
import core.Context._
import core._
import org.parboiled2._
import parser.FuseParser
import parser.FuseParser._
import parser.ParserErrorFormatter

import java.io._
import scala.util.Either
import scala.util.Failure
import scala.util.Success

object Compiler {
  def run(
      fileName: String,
      origin: InputStream,
      destination: OutputStream
  ): IO[Option[String]] =
    for {
      code <- IO.blocking(origin.readAllBytes.map(_.toChar).mkString)
      result = compile(code, fileName)
      value <- result match {
        case Right(compiledCode) =>
          IO.blocking(destination.write(compiledCode.getBytes)).map(_ => None)
        case Left(error) => IO(Some(error))
      }
    } yield value

  def compile(code: String, fileName: String): Either[Error, String] = for {
    v <- parse(code, fileName)
    c1 = BuiltIn.Functions.map(b => (b.i, NameBind))
    // NOTE: The built-in functions are reversed in order to initialize the
    // context in the correct order.
    d <- Desugar.run(v.toList, c1.reverse)
    b2 = BuiltIn.Functions ++ d
    bindings <- TypeChecker.run(b2)
    grinCode <- Right(Grin.generate(bindings))
  } yield grinCode

  def parse(code: String, fileName: String): Either[Error, Seq[FDecl]] = {
    val parser = new FuseParser(code, fileName)
    parser.Module.run() match {
      case Success(result) => Right(result)
      case Failure(e: ParseError) =>
        Left(parser.formatError(e, new ParserErrorFormatter(fileName)))
      case Failure(e) => Left(e.toString)
    }
  }
}
