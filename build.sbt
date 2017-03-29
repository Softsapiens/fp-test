organization := "com.agilogy"

name := "fptest"

resolvers += Resolver.url("Agilogy Scala", url("http://dl.bintray.com/agilogy/scala/"))(Resolver.ivyStylePatterns)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.agilogy" %% "either-extras" % "0.2.rc1"
)
