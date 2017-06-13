lazy val metaVersion = "1.6.0"
scalaVersion in ThisBuild := "2.12.2"



lazy val common = Seq(
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeIvyRepo("releases")
  )
)

lazy val macrosSetting = Seq(
  scalacOptions := {
    Seq("-Xprint:frontend,parser,macrosTransform", "-Ycheck:all")
  },
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M9" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  traceLevel := 0,

  libraryDependencies ++= Seq(
    "org.scalameta" %% "scalameta" % metaVersion
  )
) ++ common


lazy val templatingSettings = Seq(
  name := "dottydoc-templating",
  version := "0.0.1",
  organization := "vlthr"
) ++ common ++ macrosSetting

lazy val macros = (project in file("macros"))
  .settings(macrosSetting: _*)

lazy val app = (project in file("app"))
  .settings(templatingSettings)
  .dependsOn(macros)
