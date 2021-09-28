scalaVersion := "2.13.5"

name := "fuse"
version := "0.1"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.3.0"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.1"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.1.1" withSources() withJavadoc()
libraryDependencies += "com.monovore" %% "decline" % "2.0.0"
libraryDependencies += "com.monovore" %% "decline-effect" % "2.0.0"
libraryDependencies += "com.lihaoyi" %% "fansi" % "0.2.14"
libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.2" % "test"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps"
)

enablePlugins(NativeImagePlugin)

testFrameworks += new TestFramework("utest.runner.Framework")
