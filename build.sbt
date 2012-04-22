organization := "local.nodens"

name := "linkmodel"

version := "1.0"

unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.9" % "test")
              
