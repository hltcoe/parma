
import scala.util.matching.Regex
import sbt._
import Keys._

object BuildProject {
	val apachePat = """org\/apache\/commons\/collections\/FastHashMap.*""".r
	val beanPat = """.*beanutils.*""".r
}

