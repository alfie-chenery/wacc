import sbt.Keys.libraryDependencies

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val sbtAssemblySetting = baseAssemblySettings ++ Seq(
  assembly / assemblyOutputPath := baseDirectory.value / "compiler.jar",
  assembly / assemblyMergeStrategy := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case _                             => MergeStrategy.first
  }
)

lazy val root = (project in file("."))
  .settings(
    name := "Parser",
    scalaVersion :=  "2.13.8",

    sbtAssemblySetting,

    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    libraryDependencies ++= Seq("com.github.j-mie6" %% "parsley" % "3.3.4",
      "org.scalatest" %% "scalatest" % "3.2.10" % Test)
  )