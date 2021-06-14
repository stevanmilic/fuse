package fuse

import Compiler._
import cats.effect._
import cats.syntax.all._
import core.Context.Error

import java.io.File
import java.io._

object Fuse extends IOApp {

  val FuseFileExtension = "fuse"

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <-
        if (args.length < 1 && args.length > 2)
          IO.raiseError(new IllegalArgumentException("File not provided."))
        else IO.unit
      fileName = args(0)
      _ <-
        if (!fileName.endsWith(s".$FuseFileExtension"))
          IO.raiseError(
            new IllegalArgumentException(
              "File provided doesn't have the fuse extension."
            )
          )
        else IO.unit
      program = new File(fileName)
      output = new File(fileName.stripSuffix(FuseFileExtension) + "out")
      result <- compileFile(program, output)
      _ <- result match {
        case Some(error) => IO.raiseError(new RuntimeException(error))
        case None        => IO.unit
      }
    } yield ExitCode.Success

  def compileFile(origin: File, destination: File): IO[Option[Error]] = {
    val inIO: IO[InputStream] = IO(new FileInputStream(origin))
    val outIO: IO[OutputStream] = IO(new FileOutputStream(destination))

    (inIO, outIO).tupled
      .bracket { case (in, out) => compile(in, out) } { case (in, out) =>
        (IO(in.close()), IO(out.close())).tupled
          .handleErrorWith(_ => IO.unit)
          .void
      }
  }
}
