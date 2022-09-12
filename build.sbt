name := "scala-react"

organization := "github.com.ingoem"

version := "1.0"

scalaVersion := "3.2.0"

autoCompilerPlugins := true

scalacOptions ++= Seq(
	"-deprecation",
	"-unchecked",
)

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.2.13" % "test",
	"junit" % "junit" % "4.13.2" % "test",
)

