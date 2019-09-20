name := "ImmutableSOC"
organization := "io.github.soc-training-tool"
scalaVersion := "2.13.0"
description := "Library for Immutable Settlers of Catan."

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

licenses := Seq("APL2" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
homepage := Some(url("https://github.com/SOC-Training-Tool/ImmutableSOC"))
scmInfo := Some(ScmInfo(url("https://github.com/SOC-Training-Tool/ImmutableSOC"), "git@github.com:SOC-Training-Tool/ImmutableSOC.git"))

developers := List(
  Developer(
    id    = "grogdotcome",
    name  = "Gregory Herman",
    email = "g.herman27@gmail.com",
    url   = url("https://github.com/grogdotcom")
  )
)


pomIncludeRepository := { _ => false }
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
publishMavenStyle := true

pgpPublicRing := file("ci/pubring.asc")
pgpSecretRing := file("ci/secring.asc")
pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toArray)