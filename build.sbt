scalaVersion := "2.13.2"

name := "toMatrix"

libraryDependencies ++= Seq(
  "org.typelevel" %% "simulacrum" % "1.0.0",
  "org.scalanlp" %% "breeze" % "1.0",
  "org.scalanlp" %% "breeze-natives" % "1.0",
  "org.scalanlp" %% "breeze-viz" % "1.0",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "com.lihaoyi" %% "pprint" % "0.5.9",
  "org.typelevel" %% "cats-core" % "2.1.1",
  "io.higherkindness" %% "droste-core" % "0.8.0",
  "com.lihaoyi" % "ammonite" % "2.2.0" cross CrossVersion.full,
  "org.scalactic" %% "scalactic" % "3.2.0",
  "org.scalatest" %% "scalatest" % "3.2.0" % "test"
)

scalacOptions ++= Seq(
  "-Ymacro-annotations"
  //"-Xlog-implicits"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)


sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue
