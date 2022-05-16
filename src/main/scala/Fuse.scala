package fuse

import cats.effect.*
import cats.effect.*
import cats.implicits.*
import com.monovore.decline.*
import com.monovore.decline.effect.*
import core.Context.Error

import java.io.File
import java.io.*

import sys.process.*

sealed trait Command
case class BuildFile(file: String) extends Command
case class CheckFile(file: String) extends Command

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

  val buildCommand: Opts[BuildFile] =
    Opts.subcommand("build", "Compile fuse source code file.") {
      (fileOpts).map(BuildFile.apply)
    }

  val checkCommand: Opts[CheckFile] =
    Opts.subcommand("check", "Checks fuse source code file for type errors.") {
      (fileOpts).map(CheckFile.apply)
    }

  val compilerCommand = buildCommand `orElse` checkCommand

  override def main: Opts[IO[ExitCode]] =
    compilerCommand.map {
      case c: BuildFile => build(c)
      case c: CheckFile => check(c)
    }

  def build(command: BuildFile): IO[ExitCode] = {
    val fileName = command.file.stripSuffix(FuseFileExtension)
    val grinFileName = fileName + FuseGrinExtension
    val outputFileName = fileName + FuseOutputExtension
    for {
      result <- compileFile(
        command,
        new File(command.file),
        new File(grinFileName)
      )
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

  def check(command: CheckFile): IO[ExitCode] = {
    val program = new File(command.file)
    val output =
      new File(
        command.file.stripSuffix(FuseFileExtension) + FuseOutputExtension
      )
    for {
      result <- compileFile(command, program, output)
      exit_code <- result match {
        case Some(error) => IO.println(error).map(_ => ExitCode.Error)
        case None        => IO.pure(ExitCode.Success)
      }
    } yield exit_code
  }

  def compileFile(
      command: Command,
      origin: File,
      destination: File
  ): IO[Option[Error]] = {
    val inIO: IO[InputStream] = IO(new FileInputStream(origin))
    val outIO: IO[OutputStream] = IO(new FileOutputStream(destination))

    (inIO, outIO).tupled
      .bracket { case (in, out) =>
        Compiler.run(command, origin.getPath(), in, out)
      } { case (in, out) =>
        (IO(in.close()), IO(out.close())).tupled
          .handleErrorWith(_ => IO.unit)
          .void
      }
  }
}
