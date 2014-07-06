organization := "me.lessis"

name := "hipshot"

version := "0.1.0-SNAPSHOT"

crossScalaVersions := Seq("2.10.4", "2.11.1")

scalaVersion := crossScalaVersions.value.last

scalacOptions in ThisBuild ++= Seq(Opts.compile.deprecation) ++
  Seq("-Ywarn-unused-import", "-Ywarn-unused", "-Xlint", "-feature").filter(
    Function.const(scalaVersion.value.startsWith("2.11")))

libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-json4s-native" % "0.11.1")

initialCommands := "import scala.concurrent.ExecutionContext.Implicits.global"

publishMavenStyle := true

publishArtifact in Test := false

pomExtra := (
  <scm>
    <url>git@github.com:softprops/hipshot.git</url>
    <connection>scm:git:git@github.com:softprops/hipshot.git</connection>
  </scm>
  <developers>
    <developer>
      <id>softprops</id>
      <name>Doug Tangren</name>
      <url>https://github.com/softprops</url>
    </developer>
  </developers>
)
