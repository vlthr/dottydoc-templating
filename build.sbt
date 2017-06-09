lazy val dottyOrg = "me.fengy"
lazy val dottyVersion = "0.4.0-bin-SNAPSHOT"
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
    Seq("-Xprint:frontend,parser,macrosTransform", "-Ycheck:all") // "-Yplain-printer", "-Xprint:frontend,parser", "-Ylog:frontend",
  },

  traceLevel := 0,

  // Dotty version
  scalaVersion := dottyVersion,
  scalaOrganization := dottyOrg

) ++ common


lazy val templatingSettings = Seq(
  name := "dottydoc-templating",
  version := "0.0.1",
  organization := "me.vlthr",

  scalaOrganization := dottyOrg,
  scalaVersion := dottyVersion,

  libraryDependencies ++= Seq(
    ("org.scalameta" %% "scalameta" % metaVersion).withDottyCompat(),
    dottyOrg %% "dotty" % dottyVersion % "provided"
  )
) ++ common ++ macrosSetting

lazy val gestalt = Project(id="gestalt", base = file("gestalt"))

lazy val macros = (project in file("macros")).
  settings(macrosSetting: _*).
  dependsOn(gestalt)

// Use macros in this project.
lazy val app = project.settings(templatingSettings)
  .dependsOn(macros)
  .dependsOn(gestalt)
