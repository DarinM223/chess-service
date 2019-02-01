name := "chess-service"

version := "0.1"

scalaVersion := "2.12.8"

resolvers += Resolver.sonatypeRepo("releases")

val monocleVersion = "1.5.0"
val http4sVersion = "0.20.0-M5"
val tsecVersion = "0.0.1-M11"
val doobieVersion = "0.6.0"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4"),
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8"),
  compilerPlugin("org.spire-math" % "kind-projector" % "0.9.8" cross CrossVersion.binary),
  "com.github.mpilquist" %% "simulacrum" % "0.13.0",
  "io.estatico" %% "newtype" % "0.4.2",
  "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-law" % monocleVersion,

  "org.typelevel" %% "cats-core" % "1.5.0",
  "org.typelevel" %% "cats-effect" % "1.1.0",

  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,

  "io.circe" %% "circe-core" % "0.10.0",

  "io.github.jmcardon" %% "tsec-http4s" % tsecVersion,
  "io.github.jmcardon" %% "tsec-password" % tsecVersion,
  "org.tpolecat" %% "doobie-core" % doobieVersion,
  "org.tpolecat" %% "doobie-postgres" % doobieVersion,
)