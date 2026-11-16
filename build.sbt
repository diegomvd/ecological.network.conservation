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

// ── Fat JAR for headless cluster execution (Saltelli runner on Drago CSIC) ──
// `sbt assembly` produces a single self-contained JAR with all deps + the Scala
// library, so the SLURM job array only needs a JRE + this one file on the cluster.
ThisBuild / assemblyMergeStrategy := {
  case PathList("module-info.class")                    => MergeStrategy.discard
  case p if p.endsWith("/module-info.class")            => MergeStrategy.discard
  case PathList("META-INF", _ @ _*)                     => MergeStrategy.discard
  case "reference.conf"                                 => MergeStrategy.concat
  case _                                                => MergeStrategy.first
}
assembly / mainClass := Some("econetcons.SaltelliRunner")
assembly / assemblyJarName := "econetcons-saltelli.jar"

enablePlugins(SbtOsgi)

OsgiKeys.exportPackage := Seq("econetcons.*")

OsgiKeys.importPackage := Seq("*;resolution:=optional")

OsgiKeys.privatePackage := Seq("!scala.*","*")

OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""