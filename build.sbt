scalaVersion := "3.1.3-RC3"

name := "fuse"
version := "0.1"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.4.0"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.11" withSources() withJavadoc()
libraryDependencies += "com.monovore" %% "decline" % "2.2.0"
libraryDependencies += "com.monovore" %% "decline-effect" % "2.2.0"
libraryDependencies += "com.lihaoyi" %% "fansi" % "0.3.1"
libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.11" % "test"

scalacOptions ++= Seq(
  "-feature",
  "-rewrite",
  "-source:future-migration",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps"
)

enablePlugins(NativeImagePlugin)

testFrameworks += new TestFramework("utest.runner.Framework")
