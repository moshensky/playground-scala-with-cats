name := "scala-with-cats"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.8"

// scalac options come from the sbt-tpolecat plugin so need to set any here
addCompilerPlugin(
  "org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full
)

val CatsVersion = "2.8.0"
val CirceVersion = "0.14.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % CatsVersion,
  "io.circe" %% "circe-core" % CirceVersion,
  "io.circe" %% "circe-generic" % CirceVersion,
  "io.circe" %% "circe-parser" % CirceVersion,
  "org.scalameta" %% "munit" % "0.7.29" % Test,
)
