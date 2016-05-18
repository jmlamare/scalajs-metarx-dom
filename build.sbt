// Turn this project into a Scala.js project by importing these settings
enablePlugins(ScalaJSPlugin)

name := "scalajs-rx-dom"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.0",
  "pl.metastack" %%% "metarx" % "0.1.6",
  "com.lihaoyi" %%% "scalatags" % "0.4.5",
  "com.lihaoyi" %%% "scalarx" % "0.2.7"
)

bootSnippet := "jml.example.scalajsrxdom.Scalajsrxdom().main()"


