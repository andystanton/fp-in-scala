lazy val root = (project in file("."))
  .settings(
    name := "fp-in-scala",
    version := "1.0.0",
    scalaVersion := "2.12.8",
    scalacOptions += "-deprecation",

    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.0.5",
      "org.scalatest" %% "scalatest" % "3.0.5" % "test",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
      "com.lihaoyi" % "ammonite" % "1.6.4" % "test" cross CrossVersion.full
    ),

    resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases",

    sourceGenerators in Test += Def.task {
      val file = (sourceManaged in Test).value / "amm.scala"
      IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
      Seq(file)
    }.taskValue
  )
