libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.1"

addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"             % "0.14.7")
addSbtPlugin("com.github.sbt" % "sbt-release" % "1.5.0")
addSbtPlugin("com.github.sbt"       % "sbt-pgp"                  % "2.3.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.22.0")
addSbtPlugin("org.scala-js"       % "sbt-jsdependencies"       % "1.0.2")
addSbtPlugin("org.xerial.sbt"     % "sbt-sonatype"             % "3.12.2")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.3")

//addSbtPlugin("org.jmotor.sbt" % "sbt-dependency-updates" % "1.2.9")
