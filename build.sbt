organization := "com.github.tkawachi"
name := "auto-ami"
version := "0.0.1-SNAPSHOT"
scalaVersion := "2.11.4"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",        // N.B. doesn't work well with the ??? hole
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused",
  "-Ywarn-unused-import"     // 2.11 only
)

scalariformSettings
doctestSettings

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.1.2",
  "com.amazonaws" % "aws-java-sdk-ec2" % "1.9.4",
  "com.github.kxbmap" %% "configs" % "0.2.2",
  "com.github.nscala-time" %% "nscala-time" % "1.4.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
)
