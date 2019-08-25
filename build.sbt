import DependencySettings._

val settingsHelper = ProjectSettingsHelper("au.id.tmm","countstv")()

settingsHelper.settingsForBuild

lazy val root = project
  .in(file("."))
  .settings(settingsHelper.settingsForRootProject)
  .settings(console := (console in Compile in core).value)
  .aggregate(
    core,
    circe,
  )

val probabilityMeasureVersion = "0.0.1"
val tmmUtilsVersion = "0.3.1"

lazy val core = project
  .in(file("core"))
  .settings(settingsHelper.settingsForSubprojectCalled("core"))
  .settings(
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0",
    libraryDependencies += "net.sf.trove4j" % "trove4j" % "3.0.3",
    libraryDependencies += "au.id.tmm.probability-measure" %% "probability-measure-core" % probabilityMeasureVersion,
    libraryDependencies += "au.id.tmm.tmm-utils" %% "tmm-utils-collection" % tmmUtilsVersion,
    libraryDependencies += "au.id.tmm.tmm-utils" %% "tmm-utils-codec" % tmmUtilsVersion,
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.25",
    libraryDependencies += "com.google.guava" % "guava" % "28.0-jre",
  )

lazy val circe = project
  .in(file("circe"))
  .settings(settingsHelper.settingsForSubprojectCalled("circe"))
  .settings(circeDependency)
  .settings(
    libraryDependencies += "au.id.tmm.probability-measure" %% "probability-measure-circe" % probabilityMeasureVersion,
  )
  .dependsOn(core)

addCommandAlias("check", ";+test;scalafmtCheckAll")
