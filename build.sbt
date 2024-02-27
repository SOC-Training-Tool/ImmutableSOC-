
name := "ImmutableSOC"
organization := "io.github.soc-training-tool"
description := "Library for Immutable Settlers of Catan."

val akkatest = "com.typesafe.akka" %% "akka-testkit" % "2.5.23" % "test"
val akka = "com.typesafe.akka" %% "akka-actor" % "2.5.23"
val akkatyped = "com.typesafe.akka" %% "akka-actor-typed" % "2.5.23"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
libraryDependencies += "dev.zio" %% "zio" % "2.0.13"
libraryDependencies ++= Seq(akkatest, akka, akkatyped)

//inThisBuild(List(
//  // These are normal sbt settings to configure for release, skip if already defined
//  licenses := Seq("APL2" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
//  homepage := Some(url("https://github.com/SOC-Training-Tool/ImmutableSOC")),
//  scmInfo := Some(ScmInfo(url("https://github.com/SOC-Training-Tool/ImmutableSOC"), "git@github.com:SOC-Training-Tool/ImmutableSOC.git")),
//  developers := List(Developer("grogdotcom", "Gregory Herman", "g.herman27@gmail.com", url("https://github.com/grogdotcom"))),
//  credentials += Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", sys.env.getOrElse("SONATYPE_USERNAME", ""), sys.env.getOrElse("SONATYPE_PASSWORD", "")),
//
//  // These are the sbt-release-early settings to configure
//  pgpPublicRing := file("./travis/local.pubring.asc"),
//  pgpSecretRing := file("./travis/local.secring.asc"),
//  pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toArray),
//  releaseEarlyWith := SonatypePublisher
//))

lazy val root = project.in(file("."))
  .settings(
    scalaVersion := "2.13.5",
  )

//pgpPublicRing := file("./travis/local.pubring.asc")
//pgpSecretRing := file("./travis/local.secring.asc")
