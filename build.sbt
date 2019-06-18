import Dependencies._

ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "io.github.cdelmas"
ThisBuild / organizationName := "cp-experiences"

lazy val root = (project in file("."))
  .settings(
    name := "cp-experiences",
    libraryDependencies ++= Seq(oscarCp, scalaTest % Test),
    resolvers += "Artifactory" at "http://artifactory.info.ucl.ac.be/artifactory/libs-release/"
  )
