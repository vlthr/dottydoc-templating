// scalaVersion in ThisBuild := dottyLatestNightlyBuild.get
scalaVersion in ThisBuild := "2.12.3"
scalacOptions in ThisBuild ++= Seq("-Xlog-implicits")

lazy val root = Project(id = "levee", base = file("."))
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
  name := "levee",
  version := "0.1.0",
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
    "nl.big-o" % "liqp" % "0.6.7",
    "com.chuusai" %% "shapeless" % "2.3.3-SNAPSHOT",
    "de.knutwalker" %% "validation" % "0.3.0-SNAPSHOT"
  ),
  antlr4GenListener in Antlr4 := true,
  antlr4GenVisitor in Antlr4 := true,
  antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.5",
  antlr4PackageName in Antlr4 := Some("com.vlthr.levee.parser")
) ++ common ++ macrosSetting

lazy val macros = (project in file("macros"))
  .settings(macrosSetting: _*)

lazy val app = (project in file("app"))
  .settings(templatingSettings)
  .dependsOn(macros)
