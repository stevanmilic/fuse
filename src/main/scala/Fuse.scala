package fuse

import Compiler._
import cats.effect._
import cats.implicits._
import cats.effect._
import com.monovore.decline._
import com.monovore.decline.effect._
import core.Context.Error

import java.io.File
import java.io._

case class CompileFile(fuseFile: String)

object Fuse
    extends CommandIOApp(
      name = "fuse",
      header = "Fuse is a tool for managing Fuse source code.",
      version = "0.1"
    ) {

  val FuseFileExtension = "fuse"
  val FuseOutputExtension = "out"

  val fileOpts: Opts[String] = Opts.argument[String](metavar = "file")

  val compileOpts: Opts[CompileFile] =
    Opts.subcommand("build", "Compile fuse source code file.") {
      (fileOpts).map(CompileFile)
    }

  override def main: Opts[IO[ExitCode]] =
    compileOpts.map { case CompileFile(file) =>
      val program = new File(file)
      val output =
        new File(file.stripSuffix(FuseFileExtension) + FuseOutputExtension)
      for {
        result <- compileFile(program, output)
        exit_code <- result match {
          case Some(error) => IO.println(error).map(_ => ExitCode.Error)
          case None        => IO.pure(ExitCode.Success)
        }
      } yield exit_code
    }

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
