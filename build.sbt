scalaVersion := "2.13.5"

name := "fuse"
version := "0.1"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.2.1"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.0.1" withSources() withJavadoc()
libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.2" % "test"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps"
)

testFrameworks += new TestFramework("utest.runner.Framework")
