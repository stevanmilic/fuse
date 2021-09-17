package parser

import cats.Show

object Info {
  case class Location(line: Int, column: Int, lineInput: String)

  sealed trait Info
  case class FileInfo(
      location: Location,
      fileName: String
  ) extends Info
  case object UnknownInfo extends Info

  implicit val showInfo: Show[Info] =
    Show.show(info =>
      info match {
        case FileInfo(location, fileName) =>
          val padding = List.fill(location.line.toString().length)(" ").mkString
          val errorSign = fansi.Bold.On(fansi.Color.LightRed("^"))
          val errorIndicator =
            List.fill(location.column - 1)(" ").mkString + errorSign
          List(
            s"\n$padding--> ${fileName}:${location.line}:${location.column}",
            fansi.Bold.On(s"$padding |").toString(),
            fansi.Bold
              .On(s"${location.line} |")
              .toString() + s" ${location.lineInput}",
            fansi.Bold.On(s"$padding | ").toString() + errorIndicator
          ).mkString("\n")
        case UnknownInfo => ""
      }
    )

  trait ShowInfo[T] {
    def info(v: T): Info
  }

  object ShowInfo {
    def apply[A](implicit sh: ShowInfo[A]): ShowInfo[A] = sh

    implicit class ShowInfoOps[A: ShowInfo](a: A) {
      def info = ShowInfo[A].info(a)
    }

    def info[A](f: A => Info): ShowInfo[A] =
      new ShowInfo[A] {
        def info(a: A): Info = f(a)
      }
  }
}
