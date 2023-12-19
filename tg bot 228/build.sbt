name := "tg bot 228"

version := "0.1"

scalaVersion := "2.13.12"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.4.8",
  "org.http4s" %% "http4s-blaze-client" % "0.23.14",
  "org.http4s" %% "http4s-circe" % "0.23.18",
  "io.circe" %% "circe-generic" % "0.14.5",
  "com.github.pureconfig" %% "pureconfig" % "0.17.4",
  "com.github.nscala-time" %% "nscala-time" % "2.32.0",
)