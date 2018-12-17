import dependencies._

name := "CatsBook"

version := "1.0"

scalaVersion := "2.12.4"

scalacOptions ++= List("-Ypartial-unification", "-language:higherKinds")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.5")



libraryDependencies ++= Seq(
  cats.core,
  cats.effect.core,
  fs2.core,
  fs2.io,
  "org.typelevel"       %% "cats-kernel"  % cats.VERSION,
  "org.typelevel"       %% "cats-macros"  % cats.VERSION,
  scalatest
)


