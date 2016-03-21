enablePlugins(ScalaJSPlugin)

name := "fiftythree"

version := "0.1"

scalaVersion := "2.11.7"

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.4.3" % Test

testFrameworks += new TestFramework("utest.runner.Framework")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")