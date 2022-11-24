scalaVersion := "3.2.0"

name := "fuse"
version := "0.1"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.4.1"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.8" withSources () withJavadoc ()
libraryDependencies += "com.monovore" %% "decline" % "2.2.0"
libraryDependencies += "com.monovore" %% "decline-effect" % "2.2.0"
libraryDependencies += "com.lihaoyi" %% "fansi" % "0.3.1"
libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M6" % Test

scalacOptions ++= Seq(
  "-feature",
  "-rewrite",
  "-source:future-migration",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps"
)

enablePlugins(NativeImagePlugin)

testFrameworks += new TestFramework("munit.Framework")
