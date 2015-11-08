name := "seq"

version := "1.0"

scalaVersion := "2.11.7"

// packSettings
// packMain := Map("hello" -> "org.mydomain.Hello")
packAutoSettings

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.5"
