// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import java.io.File
import collection.Map
import collection.mutable.{ ArrayBuffer, HashSet }
import io.Source
import util.Random

object GVDocConverter {
	def main(args: Array[String]) {
		val home = new File("/home/hltcoe/twolfe")
		val df = new File(home, "gv-ingest/SCALE-data/ingest/gv-round1.communications.pb")
		val mf = new File(home, "gv-ingest/Global-Voices-run1/mentions.tsv")
		val af = new File(home, "gv-ingest/Global-Voices-run1/alignments.tsv")
		val docReader = new GVDocReader(df, mf, af)
		val das = docReader.getAlignedDocuments
		val n = math.min(200, das.size)
		val out = new File(home, "scale2013/parma/data/gv/round1.%d.alignments.pb".format(n))
		ConcreteUtils.serialize(Random.shuffle(das).take(n), out)
	}
}
object GVDocReaderTester {
	def main(args: Array[String]) {
		val home = new File("/home/hltcoe/twolfe")
		val df = new File(home, "gv-ingest/SCALE-data/ingest/gv-round1.communications.pb")
		val mf = new File(home, "gv-ingest/Global-Voices-run1/mentions.tsv")
		val af = new File(home, "gv-ingest/Global-Voices-run1/alignments.tsv")
		val docReader = new GVDocReader(df, mf, af)
		for(da <- docReader.getAlignedDocuments) {
			println("[test] " + Describe.docAlignment(da))
		}
	}
}

/**
 * loosely an inverse of annotation.MTurkUtils
 * reads in a set of documments (as Communications) and
 * annotations in a mentions and alignments file
 * (see google doc for documentation on file formats)
 */
class GVDocReader(val docFile: File, val mentionFile: File, val alignmentFile: File)
		extends DocumentReader[DocAlignment]
		with Logging {

	val verbose = false

	if(!docFile.exists || !docFile.isFile)
		throw new IllegalArgumentException(docFile.getPath + " doesn't exist or is not a file!")
	if(!mentionFile.exists || !mentionFile.isFile)
		throw new IllegalArgumentException(mentionFile.getPath + " doesn't exist or is not a file!")
	if(!alignmentFile.exists || !alignmentFile.isFile)
		throw new IllegalArgumentException(alignmentFile.getPath + " doesn't exist or is not a file!")
	
	def getDocs: Map[String, DocumentBuilder] = {
		log("[GVDocReader] getting documents from " + docFile.getPath)
		ConcreteWrapper.getCommunicationsFrom(docFile)
			.map(new RichConcreteDocBuilder(_))
			.map(comm => (comm.id, comm))
			.toMap
	}

	// don't worry about collapsing arg-corefs for now, just wrap in a singleton
	def getMentions: Map[String, DocLocation] = {
		log("[GVDocReader] getting mentions from " + mentionFile.getPath)
		val byId = new java.util.HashMap[String, DocLocation]
		val linePat = """(\S+)\t(predicate|argument)\t(\S+)\t(\d+)\t(\d+)\t(\d+)\t(\d+)\t(\S+)""".r
		var failures, total = 0
		for(line <- Source.fromFile(mentionFile).getLines) {
			total += 1
			line match {
				case linePat(mentionId, predArg, docId, sentIdx, startTokIdx, endTokIdx, headTokIdx, headWord) =>
					//log(mentionId + " " + predArg + " " + docId + " " + sentIdx +
					//	" " + startTokIdx + " " + endTokIdx + " " + headTokIdx + " " + headWord)
					val m = MentionBuilder.from(sentIdx.toInt, startTokIdx.toInt, endTokIdx.toInt, headTokIdx.toInt)
					val hl = if(predArg == "predicate") Predicate(m) else Argument(m)
					byId.put(mentionId, DocLocation(docId.replaceAll("http-//", "http://"), hl))
				case _ =>
					warning("bad format for line: " + line)
					failures += 1
			}
		}
		log("[GVDocReader getMentions] %d out of %d mentions failed".format(failures, total))
		import scala.collection.JavaConverters._
		byId.asScala
	}

	// don't introduce a speciate "ExplicitDocAlignment" that stores negative alignments
	// while this would be nice, it would break to many things
	// for this ingester, just check that all possible alignments have actually been
	// covered by union(sure, possible, not_aligned)

	case class GVAlignment(val hitId: String, val reportMentionId: String, val passageMentionId: String, val conf: String)

	case class DocLocation(val docId: String, val location: HasLocation)

	def getAlignments: Seq[GVAlignment] = {
		log("[GVDocReader] getting alignments from " + alignmentFile.getPath)
		val linePat = """(\S+)\t(\S+)\t(\S+)\t(sure|possible|not_aligned)""".r
		var failures, total = 0
		val buf = new ArrayBuffer[GVAlignment]
		for(line <- Source.fromFile(alignmentFile).getLines) {
			total += 1
			line match {
				case linePat(hitId, reportId, passageId, conf) =>
					//log(hitId + " " + reportId + " " + passageId + " " + conf)
					buf += GVAlignment(hitId, reportId, passageId, conf)
				case _ =>
					warning("bad format for line: " + line)
					failures += 1
			}
			if(total % 25000 == 0)
				log("[GVDocReader] read %d alignments so far".format(total))
		}
		log("[GVDocReader getAlignments] %d out of %d alignements failed".format(failures, total))
		buf.toSeq
	}
	
	override def getAlignedDocuments: Seq[DocAlignment] = {
		Profiler.startTask("GV:getAlignedDocument")
		val docs: Map[String, DocumentBuilder] = getDocs
		log(Describe.memoryUsage())

		log("[GVDocReader getAlignedDocuments] read %d documents".format(docs.size))
		val mentions: Map[String, DocLocation] = getMentions
		log(Describe.memoryUsage())
		val das = new ArrayBuffer[DocAlignment]
		for((hit, alignments) <- getAlignments.groupBy(_.hitId)) {
			if(das.size % 25 == 0) {
				log("[GVDocReader] %d hits done, cur=%s".format(das.size, hit))
				log(Describe.memoryUsage())
			}
			var report: DocumentBuilder = null
			var passage: DocumentBuilder = null
			lazy val id = "GV-r%s-p%s".format(report.id, passage.id)
			val domain = Some("GV")

			val reportAdded = new HashSet[Mention]
			val passageAdded = new HashSet[Mention]
			val sure = new ArrayBuffer[Alignment]
			val possible = new ArrayBuffer[Alignment]
			for(a <- alignments) {

				val reportDocLoc = mentions(a.reportMentionId)
				val passageDocLoc = mentions(a.passageMentionId)
				if(report == null) {
					report = docs(reportDocLoc.docId).deepCopy
					passage = docs(passageDocLoc.docId).deepCopy
				}

				// make the alignments
				val newA = (reportDocLoc.location, passageDocLoc.location) match {
					case (rp: Predicate, pp: Predicate) =>

						// add preds/args to both documents
						if(reportAdded.add(rp.location))
							report.addPredicate(rp)
						if(passageAdded.add(pp.location))
							passage.addPredicate(pp)

						new PredicateAlignment(rp, pp)

					case (ra: Argument, pa: Argument) =>

						// NOTE: singleton argument assumption
						val rac = new ArgumentCoref(ra)
						val pac = new ArgumentCoref(pa)
						if(reportAdded.add(ra.location)) {
							if(verbose)
								log("adding report argCoref=%s, anno-report-mention=%s anno-passage-mention=%s"
									.format(rac, a.reportMentionId, a.passageMentionId))
							report.addCoref(rac)
						}
						if(passageAdded.add(pa.location)) {
							if(verbose)
								log("adding passage argCoref=%s, anno-report-mention=%s anno-passage-mention=%s"
									.format(pac, a.reportMentionId, a.passageMentionId))
							passage.addCoref(pac)
						}

						new ArgCorefAlignment(rac, pac)
				}
				a.conf match {
					case "sure" => sure += newA
					case "possible" => possible += newA
					case _ => {}
				}


			}
			das += new DocAlignment(id, domain, report, passage, sure.toSet, possible.toSet)

			if(verbose) log("adding: " + Describe.docAlignment(das.last))
		}
		val r = das.toSeq
		Profiler.endTask("GV:getAlignedDocument")
		r
	}
}

