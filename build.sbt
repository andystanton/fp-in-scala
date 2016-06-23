lazy val root = (project in file(".")).
  settings(
    name := "fpinscala",
    version := "1.0.0",
    scalaVersion := "2.11.8",
    scalacOptions += "-deprecation",

    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
      "com.lihaoyi" % "ammonite-repl" % "0.6.2" % "test" cross CrossVersion.full
    ),

    initialCommands in (Test, console) := """ammonite.repl.Main.run("")"""
  )
