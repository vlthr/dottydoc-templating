lazy val metaVersion = "1.6.0"
scalaVersion in ThisBuild := "2.12.2"

lazy val root = Project(id = "tee",
                        base = file("."))
  .aggregate(macros, app)

lazy val common = Seq(
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeIvyRepo("releases")
  )
)

lazy val macrosSetting = Seq(
  // scalacOptions := {
  //   Seq("-Xprint:frontend,parser,macrosTransform", "-Ycheck:all")
  // },
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M9" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  traceLevel := 0,

  libraryDependencies ++= Seq(
    "org.scalameta" %% "scalameta" % metaVersion
  )
) ++ common


lazy val templatingSettings = antlr4Settings ++ Seq(
  name := "dottydoc-templating",
  version := "0.0.1",
  organization := "vlthr",
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1" % "test",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  antlr4GenListener in Antlr4 := true,
  antlr4GenVisitor in Antlr4 := true,
  antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.5",
  antlr4PackageName in Antlr4 := Some("vlthr.tee.parser")
) ++ common ++ macrosSetting

lazy val macros = (project in file("macros"))
  .settings(macrosSetting: _*)

lazy val app = (project in file("app"))
  .settings(templatingSettings)
  .dependsOn(macros)
