import sbt.Keys.libraryDependencies

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
  .settings(
    name := "Parser",
    scalaVersion :=  "2.13.8",

    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    libraryDependencies ++= Seq("com.github.j-mie6" %% "parsley" % "3.3.1",
      "org.scalatest" %% "scalatest" % "3.2.10" % Test)
  )