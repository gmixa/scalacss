scalaVersion := "2.12.19"

libraryDependencies += "io.argonaut" %% "argonaut-scalaz" % "6.3.10"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.8"

scalacOptions ++= Seq(
  "-unchecked", "-deprecation",
  "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")

initialCommands := "import argonaut._, Argonaut._, scalaz._, Scalaz._"

