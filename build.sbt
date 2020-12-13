name := "mismatch"

organization := "dimeder"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.10"

libraryDependencies ++= Seq(
  "org.typelevel" %% "simulacrum" % "1.0.0",
  "org.scalanlp" %% "breeze" % "1.0",
  "org.scalanlp" %% "breeze-natives" % "1.0",
  "org.scalanlp" %% "breeze-viz" % "1.0",
  "com.lihaoyi" %% "pprint" % "0.5.9",
  "org.typelevel" %% "cats-core" % "2.1.1",
  "io.higherkindness" %% "droste-core" % "0.8.0",
  "com.lihaoyi" % "ammonite" % "2.2.0" cross CrossVersion.full,
  "org.scalactic" %% "scalactic" % "3.2.0",
  "org.scalatest" %% "scalatest" % "3.2.0" % "test",
  "com.propensive" %% "magnolia" % "0.16.0",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
)

scalacOptions ++= Seq(
  //"-Xlog-implicits"
)

addCompilerPlugin( "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full )
addCompilerPlugin( "org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full )

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write( file, """object amm extends App { ammonite.Main.main(args) }""" )
  Seq( file )
}.taskValue
