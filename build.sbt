name := "markov"

version := "0.0.1"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

testOptions in Test += Tests.Argument("-oD")
