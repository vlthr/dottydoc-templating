// scalaVersion in ThisBuild := dottyLatestNightlyBuild.get
scalaVersion in ThisBuild := "0.2.0-RC1"

lazy val leveeSettings = antlr4Settings ++ Seq(
  name := "levee",
  version := "0.1.0-SNAPSHOT",
  organization := "com.vlthr",
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
    ("com.chuusai" %% "shapeless" % "2.3.3-SNAPSHOT").withDottyCompat,
    ("de.knutwalker" %% "validation" % "0.2.0").withDottyCompat
  ),
  antlr4GenListener in Antlr4 := true,
  antlr4GenVisitor in Antlr4 := true,
  antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.5",
  antlr4PackageName in Antlr4 := Some("com.vlthr.levee.parser"),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeIvyRepo("releases")
  )
)

lazy val levee = Project(id = "levee", base = file("."))
  .settings(leveeSettings)

homepage := Some(url("https://github.com/vlthr/levee"))

scmInfo := Some(
  ScmInfo(url("https://github.com/vlthr/levee"),
          "git@github.com:vlthr/levee.git"))
developers += Developer("vlthr",
                        "Valthor Halldorsson",
                        "vlthrh@gmail.com",
                        url("https://github.com/vlthr"))

licenses += ("MIT License", url(
  "http://www.opensource.org/licenses/mit-license.php"))

pomIncludeRepository := (_ => false)

publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
