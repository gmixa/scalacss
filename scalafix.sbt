ThisBuild / scalacOptions ++= {
  if (scalaVersion.value startsWith "2")
    "-Yrangepos" :: Nil
  else
    Nil
}

ThisBuild / semanticdbEnabled := true

ThisBuild / semanticdbVersion := "4.17.0"

ThisBuild / scalafixDependencies ++= Seq(
  "com.github.liancheng" %% "organize-imports" % "0.6.0"
)
