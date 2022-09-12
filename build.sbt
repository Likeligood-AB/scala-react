name := "scala-react"

organization := "github.com.ingoem"

version := "1.0"

scalaVersion := "2.11.6"

autoCompilerPlugins := true

scalacOptions ++= Seq(
	"-deprecation",
	"-unchecked",
)

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "2.2.5" % "test",
	"junit" % "junit" % "4.11" % "test",
)

