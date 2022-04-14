ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.1"

lazy val root = (project in file("."))
  .settings(
    name := "lab01",
    idePackagePrefix := Some("com.qwerfah.sort"),
    libraryDependencies := Seq(
      "org.scalactic" %% "scalactic" % "3.2.11",
      "org.scalatest" %% "scalatest" % "3.2.11" % "test",
      "org.scalanlp" % "breeze-viz_3" % "2.0.1-RC2"
    )
  )
