// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.annotation.PredArgSelector
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.util.AnnotationAligner.HalfAlignment
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.features.ReportingVerbs
import scala.collection.mutable.{ ArrayBuffer, HashSet, HashMap, Buffer }
import scala.collection.JavaConversions._
import java.io.File

object LeastOverlapReader extends DocumentReader[ParametricDocAlignment[DocumentBuilder]] {

	override def domain: String = "LeastOverlap"

	// read in documents from f1 and f2
	// go line by line in alignments.xml, make and accumulate alignments
	// every time you hit a new doc in alignments.xml, produce a DocAlignment

	override def getAlignedDocuments: Seq[ParametricDocAlignment[DocumentBuilder]] = {

		println("[LeastOverlapReader getAlignedDocuments] reading in documents...")
		val f1f = ParmaConfig.getFile("data.least-overlap.reports")
		val f2f = ParmaConfig.getFile("data.least-overlap.passages")
		Profiler.startTask("least-overlap:docs")
		val docMap = Seq(f1f, f2f).flatMap(f => {
			println(f.getCanonicalPath)
			ConcreteWrapper.getCommunicationsFrom(f)
		}).map(d => (d.getGuid.getCommunicationId, d)).toMap
		val docTime = Profiler.endTask("least-overlap:docs")
		println("[LeastOverlapReader getAlignedDocuments] done, read in %d documents in %.1f seconds"
			.format(docMap.size, docTime / 1000d))

		val Doc = """<DOC id="(\S+)">""".r
		val Ignore = """(</?TEXT>|</DOC>)""".r
		val Alignment = """(\d+)-(\d+)""".r

		val alignments = new ArrayBuffer[ParametricDocAlignment[DocumentBuilder]]
		var alignmentSentence = 0	// running sentence index w.r.t. alignments

		var report: DocumentBuilder = null
		val reportHAs = new ArrayBuffer[HalfAlignment[Argument]]

		var passage: DocumentBuilder = null
		val passageHAs = new ArrayBuffer[HalfAlignment[Argument]]

		//val semiAlignments = new ArrayBuffer[SemiAlignment]

		// HIGH LEVEL IDEA:
		// 1) read in doc id, look up documents
		// 2) call PredArgSelector, identify preds/args
		// 3) step through alignments, if matches existing pred/arg make HalfAlignment
		// 4) call AnnotationAligner.makeDocAlignment using HalfAlignments accumulated
		val af = ParmaConfig.getFile("data.least-overlap.alignments")
		var corefSet = 0
		val r = FileUtils.getReader(af)
		while (r.ready) {
			r.readLine.trim match {

				case Ignore(s) => {}

				// ==== NEW DOC PAIR ====
				case Doc(id) => { // ids in alignment file don't have .a and .b extensions

					assert((report == null) == (passage == null))
					if (report != null) {
						// dump previous alignment
						// 4) call AnnotationAligner.makeDocAlignment using HalfAlignments accumulated
						alignments += AnnotationAligner.makeDocAlignment(
							report, reportHAs.toSeq,
							passage, passageHAs.toSeq,
							Some(domain), addPredArgs=false, strict=false)
					}

					// 1) read in doc id, look up documents
					// 2) call PredArgSelector, identify preds/args
					report = new RichConcreteDocBuilder(id + ".a", docMap(id + ".a"))
					report = PredArgSelector.identifyPredicatesAndArguments(report)
					reportHAs.clear

					passage = new RichConcreteDocBuilder(id + ".b", docMap(id + ".b").toBuilder.build)
					passage = PredArgSelector.identifyPredicatesAndArguments(passage)
					passageHAs.clear

					alignmentSentence = 0
					corefSet = 0
				}

				// ==== ALIGNMENT LINE ====
				case s => {
					assert(report != null && passage != null)

					// 3) step through alignments, if matches existing pred/arg make HalfAlignment
					Alignment.findAllMatchIn(s).foreach(m => {
						val rTokIdx = m.group(1).toInt
						val pTokIdx = m.group(2).toInt

						// we are assuming these are all sure alignments because
						// we have no scores to threshold on
						val isSure = true
						corefSet += 1
						
						// REPORT HalfAlignment
						//val rPredArg = report.predOrArgMatching(MentionBuilder.from(alignmentSentence, rTokIdx))
						val rPredArg = predOrArgMatching(MentionBuilder.from(alignmentSentence, rTokIdx), report)
						rPredArg match {
							case Some(epa) =>
								//println("[LeastOverlap] adding REPORT HA: " + epa)
								reportHAs += new HalfAlignment(epa, corefSet.toString, isSure)
							case None => {}	// no pred or arg matching this token
						}

						// PASSAGE HalfAlignment
						//val pPredArg = passage.predOrArgMatching(MentionBuilder.from(alignmentSentence, pTokIdx))
						val pPredArg = predOrArgMatching(MentionBuilder.from(alignmentSentence, pTokIdx), passage)
						pPredArg match {
							case Some(epa) =>
								//println("[LeastOverlap] adding PASSAGE HA: " + epa)
								passageHAs += new HalfAlignment(epa, corefSet.toString, isSure)
							case None => {}	// no pred or arg matching this token
						}
					})
					alignmentSentence += 1
				}
			}
		}
		r.close

		alignments.toSeq
	}

	def predOrArgMatching(m: Mention, d: Document): Option[Either[Predicate, Argument]] = {
		val p = d.predicates.filter(_.location == m).map(Left(_))
		val a = d.arguments.filter(_.location == m).map(Right(_))
		(a ++ p).headOption
	}

}


