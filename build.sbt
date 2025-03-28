ThisBuild / scalaVersion := "3.6.3"

// ! This should be fixed, it's a workaround for now.
// ! Using add CompilerPlugin doesn't work.
val scinearPluginPath =
  "/home/amirhossein/.ivy2/local/ca.uwaterloo.plg/scinear-plugin_3/0.1.0-SNAPSHOT/jars/scinear-plugin_3.jar"

lazy val root = project
  .in(file("."))
  .settings(
    name := "imem",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies := Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "ca.uwaterloo.plg" %% "scinear-lib" % "0.1.0-SNAPSHOT"
    ),
    scalacOptions += s"""-Xplugin:$scinearPluginPath"""
  )
