import AssemblyKeys._ // for sbt assembly

import BuildProject._

assemblySettings

// vandurme: an example build.sbt file with many more options can be found at:
// http://www.scala-sbt.org/release/docs/Examples/Quick-Configuration-Examples

// vandurme: for cases such as highside deployment, otherwise don't use please

resolvers ++= Seq(
  "HLTCOE Maven Repository libs snapshots" at "http://test1:8081/artifactory/repo"
)

//  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
//  "Scala-tools Maven2 Repository" at "http://scala-tools.org/repo-releases",
//  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
//  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
//  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
//  "sonatype-public" at "https://oss.sonatype.org/content/groups/public",
//  "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

name := "parma"

version := "1.2-SNAPSHOT"

scalaVersion := "2.10.2"

// set the main Scala source directory to be <base>/src
scalaSource in Compile <<= baseDirectory(_ / "src")

// set the Scala test source directory to be <base>/test
scalaSource in Test <<= baseDirectory(_ / "test")

libraryDependencies in Test += "junit" % "junit" % "3.8.1"



// sbt-assembly stuff
jarName in assembly := "parma-via-assembly.jar"

test in assembly := {}

mainClass in assembly := Some("edu.jhu.hlt.parma.experiments.PipelineRunner")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { old => {
	case PathList("org", "apache", "commons", xs @ _*) =>
		MergeStrategy.last
    case x =>
		old(x)
  }
}

// omg sbt...
// make sure that you only put one % sign between these dependencies
// if you want them to obey exactly what you write
// if you type "foo" %% "bar" %% "0.1", this will be translated
// into "foo" % "bar_$SCALA_VERSION" % "0.1"
// https://github.com/harrah/xsbt/wiki/Getting-Started-Library-Dependencies

libraryDependencies ++= Seq(
  "edu.jhu.hlt.concrete" % "concrete-protobufs" % "1.1.6",
  "edu.jhu.hlt.concrete" % "concrete-java" % "1.1.6",
  "edu.jhu.jerboa" % "jerboa" % "1.0.1",
  "edu.jhu.coe.cale" % "cale" % "1.0",
  "com.google.protobuf" % "protobuf-java" % "2.5.0",
  "com.google.guava" % "guava" % "14.0.1",
  "org.scala-lang" % "scala-compiler" % "2.10.2",
  "redis.clients" % "jedis" % "2.0.0",
  "org.apache.commons" % "commons-math3" % "3.0",
  "commons-cli" % "commons-cli" % "1.2",
  "commons-io" % "commons-io" % "2.4",
  "commons-configuration" % "commons-configuration" % "1.7",
  "cc.mallet" % "mallet" % "2.0.7-RC2",
  "edu.mit" % "jwi" % "2.2.3",
  "colt" % "colt" % "1.2.0",
  "no.priv.garshol.duke" % "duke" % "1.0")

  //"ac.biu.nlp.normalization" % "normalization" % "0.6.1")

// set the prompt (for this build) to include the project name
shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

// time between checks to whether files have been saved. Value is in
// milliseconds, default is 500
pollInterval := 500

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")

scalacOptions += "-deprecation"

scalacOptions += "-feature"

scalacOptions += "-language:postfixOps"

scalacOptions += "-language:implicitConversions"

scalacOptions in (Compile, console) += "-Yrepl-sync"

mainClass in (Compile, packageBin) := Some("edu.jhu.parma.CLI")

// I want to be able to move a source file from src/main/ into the
// root of the project and have sbt not compile it.
// This means that sbt will only compile files in the src/ directory.
sources in (Compile, compile) ~= (_ filter (_.getPath contains "src/"))

// in console it is nice to not have to import stuff over-and-over again
initialCommands := """
  import edu.jhu.hlt.parma.types._
  import edu.jhu.hlt.parma.util._
  import edu.jhu.hlt.parma.math._
  import edu.jhu.hlt.parma.input._
  import edu.jhu.hlt.parma.inference._
  import edu.jhu.hlt.parma.experiments._
  import edu.jhu.hlt.parma.features._
  import edu.jhu.hlt.parma.evaluation._
  import edu.jhu.hlt.parma.feature_interfaces._
  import edu.jhu.hlt.parma.diagnostics._
  import edu.jhu.hlt.parma.annotation._
  import java.io.File
"""


