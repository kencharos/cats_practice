name := "catz_start"

version := "1.0"

scalaVersion := "2.12.2"
//scalaVersion := "0.2.0-RC1"

libraryDependencies := Seq(
    "org.typelevel" %% "cats-core" % "1.0.0-MF",
    "org.typelevel" %% "cats-macros" % "1.0.0-MF",
    "org.typelevel" %% "cats-free" % "1.0.0-MF",
    "org.typelevel" %% "cats-testkit" % "1.0.0-MF",
    "org.typelevel" %% "cats-effect" % "0.4")
