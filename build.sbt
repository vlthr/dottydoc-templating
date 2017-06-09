lazy val dottyOrg = "ch.epfl.lamp"
lazy val dottyVersion = "0.1.2-RC1"
lazy val metaVersion = "1.6.0"



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

  traceLevel := 0,

  scalaVersion := dottyVersion,
  scalaOrganization := dottyOrg,

  libraryDependencies ++= Seq(
    ("org.scalameta" %% "scalameta" % metaVersion).withDottyCompat(),
    ("my.fengy" %% "gestalt" % "0.1.1").withDottyCompat(),
    dottyOrg %% "dotty" % dottyVersion % "provided"
  )
) ++ common


lazy val templatingSettings = Seq(
  name := "dottydoc-templating",
  version := "0.0.1",
  organization := "vlthr",

  scalaOrganization := dottyOrg,
  scalaVersion := dottyVersion
) ++ common ++ macrosSetting


lazy val macros = (project in file("macros"))
  .settings(macrosSetting: _*)

lazy val app = (project in file("app"))
  .settings(templatingSettings)
  .dependsOn(macros)
