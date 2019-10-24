import Dependencies._

inThisBuild(List(
  scalaVersion     := "2.13.1",
  version          := "0.1.0-SNAPSHOT",
  organization     := "com.example",
  organizationName := "example",
  homepage := Some(url("https://github.com/tksfz/ensuring")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "tksfz",
      "Thomas Kim",
      "tom+github@tksfz.org",
      url("https://github.com/tksfz")
    )
  )))

lazy val root = Project("ensuring", file("."))
  .settings(name := "ensuring")
  .aggregate(
    core,
    aws,
  )

lazy val core = Project("ensuring-core", file("ensuring-core"))
  .settings(
    name := "ensuring-core",
    libraryDependencies += scalaTest % Test,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.0.0",
      "org.typelevel" %% "cats-effect" % "2.0.0",
    )
  )

lazy val aws = Project("ensuring-aws", file("ensuring-aws"))
  .settings(
    name := "ensuring-aws",
    libraryDependencies += scalaTest % Test,
    libraryDependencies ++= Seq(
      "software.amazon.awssdk" % "aws-sdk-java" % "2.9.23",
    )
  )
  .dependsOn(core % "compile->compile;test->test")

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
