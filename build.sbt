organization := "com.github.tkawachi"

name := "auto-ami"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.4"

scalariformSettings

doctestSettings

libraryDependencies ++= Seq(
  "com.amazonaws" % "aws-java-sdk-ec2" % "1.9.4"
)
