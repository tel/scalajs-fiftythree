

lazy val common = Seq(
  version := "0.1",
  scalaVersion := "2.11.7",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
)

lazy val lib = (project in file("lib"))
  .settings(common: _*)
  .settings(
    name := "fiftythree",
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.4.3" % Test,
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .enablePlugins(ScalaJSPlugin)

lazy val example = (project in file("example"))
  .settings(workbenchSettings: _*)
  .settings(common: _*)
  .settings(
    name := "fiftythree-example",
    bootSnippet := "tel.fiftythree.App().main();",
    updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile)
  )
  .dependsOn(lib)
  .enablePlugins(ScalaJSPlugin)
