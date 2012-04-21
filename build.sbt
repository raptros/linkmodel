organization := "nodens.local"

name := "linkmodel"

version := "0.1-SNAPSHOT"

unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.9" % "test")
              
