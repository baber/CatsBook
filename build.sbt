name := "CatsBook"

version := "1.0"

scalaVersion := "2.12.4"

scalacOptions ++= List("-Ypartial-unification", "-language:higherKinds")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.5")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.1",
  "org.typelevel" %% "cats-kernel" % "1.0.1",
  "org.typelevel" %% "cats-macros" % "1.0.1"
)
    
