resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.1.3")
