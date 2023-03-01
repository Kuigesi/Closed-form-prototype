lazy val root = (project in file("."))
  .settings(
    name := "prototype",
    scalaVersion := "3.2.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test,
  )