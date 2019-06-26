name := "gridcomponent"

version := "0.1"

scalaVersion := "2.12.5"  //6

enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)

dependencyOverrides in ThisBuild += "org.webjars.npm" % "js-tokens" % "3.0.2"

libraryDependencies ++= Seq(
  "io.github.outwatch" %%% "outwatch" % "0.11.1-SNAPSHOT",
  "com.chuusai" %%% "shapeless" % "2.3.3",
)