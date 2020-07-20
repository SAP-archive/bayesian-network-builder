import Dependencies._

ThisBuild / scalaVersion := "2.13.3"
ThisBuild / version := "0.1"
ThisBuild / organization := "com.sap"
ThisBuild / organizationName := "bayesian-network-builder"
resolvers += Resolver.mavenLocal
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
lazy val root = (project in file("."))
  .settings(
    name := "bayesian-network-builder",
    libraryDependencies += scalaTest % Test,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.1",
      "org.apache.logging.log4j" % "log4j-core" % "2.12.1",
      "org.apache.logging.log4j" % "log4j-api" % "2.12.1",
      "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.12.1",
    )
  )
