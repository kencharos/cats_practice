name := "cats_practice"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies := Seq(
    "org.typelevel" %% "cats-core" % "1.0.0-MF",
    "org.typelevel" %% "cats-macros" % "1.0.0-MF",
    "org.typelevel" %% "cats-free" % "1.0.0-MF",
    "org.typelevel" %% "cats-testkit" % "1.0.0-MF",
    "org.typelevel" %% "cats-effect" % "0.4")

// Kind 解決のためのプラグイン
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
// マクロでボイラープレート生成のためのプラグイン
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.10.0"