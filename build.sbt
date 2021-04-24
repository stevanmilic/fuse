scalaVersion := "2.13.3"

name := "fuse"
version := "0.1"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.2.1"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.2" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")
