scalaVersion := "2.13.2"

name := "toMatrix"

libraryDependencies ++= Seq(
  "org.typelevel" %% "simulacrum" % "1.0.0",
  "org.scalanlp" %% "breeze" % "1.0",
  "org.scalanlp" %% "breeze-natives" % "1.0",
  "org.scalanlp" %% "breeze-viz" % "1.0",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "com.lihaoyi" %% "pprint" % "0.5.9"
  )

scalacOptions ++= Seq(
        //"-Xlog-implicits"
      )
