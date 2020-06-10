scalaVersion := "2.12.10"

name := "toMatrix"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.typelevel" %% "simulacrum" % "1.0.0",
  "org.scalanlp" %% "breeze" % "1.0",
  "org.scalanlp" %% "breeze-natives" % "1.0",
  "org.scalanlp" %% "breeze-viz" % "1.0",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "com.lihaoyi" %% "pprint" % "0.5.9"
  )

scalacOptions ++= Seq(
        "-Xfatal-warnings", // to make deprecation warnings errors in the build
        "-deprecation",
        "-feature",
        "-Ywarn-dead-code",
        "-Ywarn-inaccessible",
        "-Ywarn-infer-any",
        "-Ywarn-nullary-override",
        "-Ywarn-nullary-unit",
        "-Ywarn-numeric-widen",
        //"-Ywarn-unused",
        "-Xmax-classfile-name",
        "128",
        "-language:existentials",
        "-language:experimental.macros",
        "-Xlog-implicits"
      )
