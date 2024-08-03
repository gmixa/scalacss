scalaVersion := "2.13.14"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.3.8"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.12"

scalacOptions ++= Seq(
  "-unchecked", "-deprecation",
  "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")

initialCommands := "import scalaz._, shapeless._, shapeless.syntax.singleton._"

