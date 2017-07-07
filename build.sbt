lazy val metaVersion = "1.6.0"
// scalaVersion in ThisBuild := dottyLatestNightlyBuild.get
scalaVersion in ThisBuild := "0.1.2-RC1"

lazy val root = Project(id = "tee", base = file("."))
  .aggregate(macros, app)

lazy val common = Seq(
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeIvyRepo("releases")
  )
)

lazy val macrosSetting = Seq(
  ) ++ common

lazy val templatingSettings = antlr4Settings ++ Seq(
  name := "dottydoc-templating",
  version := "0.0.1",
  organization := "vlthr",
  libraryDependencies ++= Seq(
    "junit" % "junit" % "4.12" % "test",
    "com.novocode" % "junit-interface" % "0.11" % "test",
    "com.vladsch.flexmark" % "flexmark" % "0.11.1",
    "com.vladsch.flexmark" % "flexmark-ext-gfm-tasklist" % "0.11.1",
    "com.vladsch.flexmark" % "flexmark-ext-gfm-tables" % "0.11.1",
    "com.vladsch.flexmark" % "flexmark-ext-autolink" % "0.11.1",
    "com.vladsch.flexmark" % "flexmark-ext-anchorlink" % "0.11.1",
    "com.vladsch.flexmark" % "flexmark-ext-emoji" % "0.11.1",
    "com.vladsch.flexmark" % "flexmark-ext-gfm-strikethrough" % "0.11.1",
    "com.vladsch.flexmark" % "flexmark-ext-yaml-front-matter" % "0.11.1",
    "nl.big-o" % "liqp" % "0.7.0"
  ),
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

lazy val genTestExamples =
  inputKey[Unit]("Generate test outputs from examples")
