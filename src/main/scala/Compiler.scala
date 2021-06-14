package fuse

import cats.data.EitherT
import cats.effect.IO
import cats.data.State
import cats.implicits._
import core.Context._
import core._
import org.parboiled2._
import parser.FuseParser
import parser.FuseParser._

import java.io._
import scala.util.Either
import scala.util.Failure
import scala.util.Success

object Compiler {
  def compile(
      origin: InputStream,
      destination: OutputStream
  ): IO[Option[Error]] =
    for {
      code <- IO.blocking(origin.readAllBytes.map(_.toChar).mkString)
      result = process(code)
      value <- result match {
        case Right(compiledCode) =>
          IO.blocking(destination.write(compiledCode.getBytes)).map(_ => None)
        case Left(error) => IO(Some(error))
      }
    } yield value

  def process(code: String): Either[Error, String] = for {
    v <- parse(code)
    c1 = BuiltIn.Functions.map(b => (b.i, NameBind))
    d <- Right(desugar(v.toList, c1))
    b2 = BuiltIn.Functions ++ d
    types <- typeCheck(b2)
    repr <- typeRepresentation(types)
  } yield repr.mkString("\n")

  def parse(code: String): Either[String, Seq[FDecl]] = {
    val parser = new FuseParser(code)
    parser.Module.run() match {
      case Success(result) => Right(result)
      case Failure(e: ParseError) =>
        Left(parser.formatError(e, new ErrorFormatter(showTraces = false)))
      case Failure(e) => Left(e.toString)
    }
  }

  def desugar(decls: List[FDecl], initContext: Context): List[Bind] =
    Desugar.process(decls).runA(initContext).value

  def typeCheck(
      binds: List[Bind]
  ): Either[Error, List[Bind]] =
    TypeChecker
      .check(binds)
      .value
      .runA(empty)
      .value

  def typeRepresentation(
      types: List[Bind]
  ): Either[Error, List[String]] =
    types
      .traverse(bind => {
        val repr = bind.b match {
          case TypeVarBind(k) =>
            Representation.kindToString(k).pure[StateEither]
          case VarBind(ty) => Context.runE(Representation.typeToString(ty))
          case TypeAbbBind(_, Some(k)) =>
            Representation.kindToString(k).pure[StateEither]
          case TermAbbBind(_, Some(ty)) =>
            Context.runE(Representation.typeToString(ty))
        }
        repr.map2(EitherT.liftF(addName(bind.i))) { case (repr, id) =>
          s"$id: $repr"
        }
      })
      .value
      .runA(empty)
      .value

}
