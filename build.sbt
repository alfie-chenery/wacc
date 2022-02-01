Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
  .settings(
    name := "Parser",
    scalaVersion :=  "2.13.8",

    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    libraryDependencies += "com.github.j-mie6" %% "parsley" % "3.3.1"
  )