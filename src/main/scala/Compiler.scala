package fuse

import cats.data.EitherT
import cats.data.State
import cats.effect.IO
import cats.implicits.*
import code.Grin
import core.Bindings.*
import core.Context.*
import core.*
import org.parboiled2.*
import parser.FuseParser
import parser.FuseParser.*
import parser.ParserErrorFormatter

import java.io.*
import scala.util.Either
import scala.util.Failure
import scala.util.Success

object Compiler {
  def run(
      command: Command,
      fileName: String,
      origin: InputStream,
      destination: OutputStream
  ): IO[Option[String]] =
    for {
      code <- IO.blocking(origin.readAllBytes.map(_.toChar).mkString)
      result = compile(command, code, fileName)
      value <- result match {
        case Right(compiledCode) =>
          IO.blocking(destination.write(compiledCode.getBytes)).map(_ => None)
        case Left(error) => IO(Some(error))
      }
    } yield value

  def compile(
      command: Command,
      code: String,
      fileName: String
  ): Either[Error, String] = for {
    v <- parse(code, fileName)
    c1 = BuiltIn.Functions.map(b => (b.i, NameBind))
    // NOTE: The built-in functions are reversed in order to initialize the
    // context in the correct order.
    d <- Desugar.run(v.toList, (c1.reverse, 0))
    b2 = BuiltIn.Functions ++ d
    bindings <- TypeChecker.run(b2)
    code <- command match {
      case BuildFile(_) => Right(Grin.generate(bindings))
      case CheckFile(_) =>
        Representation.typeRepresentation(bindings).map(_.mkString("\n"))
    }
  } yield code

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
