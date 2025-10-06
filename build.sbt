ThisBuild / scalaVersion := "3.7.3"

// WARN: This should be fixed, it's a workaround for now.
// WARN: Using add CompilerPlugin doesn't work.
val scinearPluginPath =
  "/home/amirhossein/.ivy2/local/ca.uwaterloo.plg/scinear-plugin_3/0.2.1-SNAPSHOT/jars/scinear-plugin_3.jar"

lazy val root = project
  .in(file("."))
  .settings(
    name := "imem",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies += "ca.uwaterloo.plg" %% "scinear-lib" % "0.2.1-SNAPSHOT",
    scalacOptions += "-explain",
    // scalacOptions += "-explain-cyclic",
    // scalacOptions += "-Ydebug-cyclic",
    // scalacOptions += s"""-Xplugin:$scinearPluginPath"""
    // scalacOptions += "-Xprint:cc"
    // scalacOptions += "-Ycc-debug"
  )
