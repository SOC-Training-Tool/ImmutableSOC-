
name := "ImmutableSOC"
organization := "io.github.soc-training-tool"
scalaVersion := "2.13.0"
description := "Library for Immutable Settlers of Catan."

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies ++= Seq(
  "io.grpc" % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion,
  "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion
)

PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value
)

inThisBuild(List(
  // These are normal sbt settings to configure for release, skip if already defined
  licenses := Seq("APL2" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("https://github.com/SOC-Training-Tool/ImmutableSOC")),
  scmInfo := Some(ScmInfo(url("https://github.com/SOC-Training-Tool/ImmutableSOC"), "git@github.com:SOC-Training-Tool/ImmutableSOC.git")),
  developers := List(Developer("grogdotcome", "Gregory Herman", "g.herman27@gmail.com", url("https://github.com/grogdotcom"))),
  credentials += Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", sys.env.getOrElse("SONATYPE_USERNAME", ""), sys.env.getOrElse("SONATYPE_PASSWORD", "")),

  // These are the sbt-release-early settings to configure
  pgpPublicRing := file("./travis/local.pubring.asc"),
  pgpSecretRing := file("./travis/local.secring.asc"),
  pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toArray),
  releaseEarlyWith := SonatypePublisher
))



//pgpPublicRing := file("./travis/local.pubring.asc")
//pgpSecretRing := file("./travis/local.secring.asc")
