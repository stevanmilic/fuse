package fuse

import cats.effect._
import cats.effect._
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import core.Context.Error

import java.io.File
import java.io._

import sys.process._

case class CompileFile(fuseFile: String)

object Fuse
    extends CommandIOApp(
      name = "fuse",
      header = "Fuse is a tool for managing Fuse source code.",
      version = "0.1"
    ) {

  val FuseFileExtension = "fuse"
  val FuseGrinExtension = "grin"
  val FuseOutputExtension = "out"
  val GrinRuntimeFile = "grin/runtime.c"
  val GrinPrimOpsFile = "grin/prim_ops.c"

  val fileOpts: Opts[String] = Opts.argument[String](metavar = "file")

  val compileOpts: Opts[CompileFile] =
    Opts.subcommand("build", "Compile fuse source code file.") {
      (fileOpts).map(CompileFile)
    }

  override def main: Opts[IO[ExitCode]] =
    compileOpts.map { case CompileFile(file) =>
      val fileName = file.stripSuffix(FuseFileExtension)
      val grinFileName = fileName + FuseGrinExtension
      val outputFileName = fileName + FuseOutputExtension
      for {
        result <- compileFile(new File(file), new File(grinFileName))
        fuseExitCode <- result match {
          case Some(error) => IO.println(error).map(_ => ExitCode.Error)
          case None        => IO.pure(ExitCode.Success)
        }
        grinExitCode <- fuseExitCode match {
          case ExitCode.Success =>
            IO.blocking(
              s"grin $grinFileName --optimize -o $outputFileName -q -C $GrinRuntimeFile -C $GrinPrimOpsFile" !
            ).map(_ match {
              case 0 => ExitCode.Success
              case _ => ExitCode.Error
            })
          case c => IO.pure(c)
        }
      } yield grinExitCode
    }

  def compileFile(origin: File, destination: File): IO[Option[Error]] = {
    val inIO: IO[InputStream] = IO(new FileInputStream(origin))
    val outIO: IO[OutputStream] = IO(new FileOutputStream(destination))

    (inIO, outIO).tupled
      .bracket { case (in, out) => Compiler.run(origin.getName(), in, out) } {
        case (in, out) =>
          (IO(in.close()), IO(out.close())).tupled
            .handleErrorWith(_ => IO.unit)
            .void
      }
  }
}
