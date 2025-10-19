import scala.collection.Seq

ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "3.3.3"


lazy val root = (project in file("."))
  .settings(
    name := "econetcons",
    libraryDependencies += "org.scala-lang" %% "scala3-library" % "3.1.0",
    libraryDependencies += "org.jgrapht" % "jgrapht-core" % "1.5.2",
    libraryDependencies += "org.locationtech.jts" % "jts-core" % "1.19.0",
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  )

enablePlugins(SbtOsgi)

OsgiKeys.exportPackage := Seq("econetcons.*")

OsgiKeys.importPackage := Seq("*;resolution:=optional")

OsgiKeys.privatePackage := Seq("!scala.*","*")

OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""