// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.util.AnnotationAligner.HalfAlignment
import edu.jhu.hlt.parma.diagnostics.GeneralDiagnostics
import edu.jhu.hlt.concrete.Concrete.Communication
import edu.jhu.hlt.concrete.io.ProtocolBufferReader
import scala.io._
import scala.collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Buffer}
import java.io._
import java.util.zip.GZIPInputStream

/**
 * use this class to read in DocAlignments via RothFrankDocReader (slow)
 * and serializing them to a Concrete protobuf file so they can
 * be read in with ConcreteDocReader (fast)
 */
object RothFrankConverter {
	def main(args: Array[String]) {
		if(args.length != 1) {
			println("please provide a parma.config file")
			return
		}
		ParmaConfig.load(args(0))
		val outfile = ParmaConfig.getFile("data.conversion.output")
		println("[RothFrankConverter] writing Roth and Frank data into " + outfile.getPath)
		val start = System.currentTimeMillis
		val das = new RothFrankDocReader(true, true).getAlignedDocuments
		println("[RothFrankConverter] read in %d documents in %.1f seconds, serializing them to %s"
			.format(das.size, (System.currentTimeMillis-start)/1000d, outfile.getPath))
		ConcreteUtils.serialize(das, outfile)

		// write out communications after the preds and args have been added
		val commsWithPredArgs = new File("data/roth_frank/rf-docs-annotations-concrete.pb")
		ConcreteWrapper.writeCommunicationsTo(commsWithPredArgs, das.flatMap(da => Seq(da.report, da.passage)).toSet[CommunicationDocument].map(_.communication).toSeq)

		println("[RothFrankConverter] done, converted %d documents in %.1f seconds"
			.format(das.size, (System.currentTimeMillis-start)/1000d))
	}
}

class RothFrankDocReader(var takeDev: Boolean, var takeTest: Boolean)
		extends DocumentReader[ParametricDocAlignment[RichConcreteDocBuilder]]
		with Logging {
	
	def this() = this(true, false)
	
	val pedantic = false

	override def domain = "RF"
	
	override def getAlignedDocuments: Seq[ParametricDocAlignment[RichConcreteDocBuilder]] = {
		val docStore = ConcreteWrapper.getCommunicationsFrom(ParmaConfig.getFile("data.roth_frank.concrete.documents"))
			.map(c => (c.getGuid.getCommunicationId -> c)).toMap
		val annotationFile = ParmaConfig.getFile("data.roth_frank.annotations")
		val alignments = new ArrayBuffer[ParametricDocAlignment[RichConcreteDocBuilder]]
		val source = Source.fromFile(annotationFile)
		val iter = source.getLines
		
		// oh imperative programming!
		var pairDir = iter.next
		var report, passage: RichConcreteDocBuilder = null
		var reportMentions, passageMentions: Buffer[HalfAlignment[Argument]] = null
		var oldPairDir: String = null
		var reportName: String = null
		var passageName: String = null

		if(pedantic && !(takeDev ^ takeTest))
			warning("you probably don't want to take both train *and* test alignments from this data!")
		
		val f2n = (f: String) => f.replace("XML/", "").replace("_", "").replace("/", ".")
		
		while(pairDir != null) {
			
			reportName = iter.next
			report = new RichConcreteDocBuilder(f2n(pairDir+"1"), docStore(reportName))
			
			reportMentions = new ArrayBuffer[HalfAlignment[Argument]]
			passageName  = readAlignments(iter, report, reportMentions)		// add alignments from 1.xml
			
			passage = new RichConcreteDocBuilder(f2n(pairDir+"2"), docStore(passageName))
			
			oldPairDir = pairDir
			passageMentions = new ArrayBuffer[HalfAlignment[Argument]]
			pairDir = readAlignments(iter, passage, passageMentions)			// add alignments from 2.xml
			
			
			// TODO
			// there is some dependency in what you set the document name
			// to and the mentions that get added. if i run the code as-is
			// (i.e. with new Document(f2n(pairDir+"1"), docStore(aDoc1Name)))
			// then it works. if i put it back to how i had it for chris
			// and MTurkUtils, it breaks. fix this.
			
			
			val da = AnnotationAligner.makeDocAlignment(report, reportMentions, passage, passageMentions, Some(domain), addPredArgs=true)
			if(takeDev && oldPairDir.contains("dev"))
				alignments += da
			if(takeTest && oldPairDir.contains("test"))
				alignments += da
		}
		source.close
		
		GeneralDiagnostics.printDocAlignmentStatistics(alignments, "debug, takeDev=%s takeTest=%s".format(takeDev, takeTest))
		alignments.toSeq
	}
	
	
	/**
	 * calls addAPredArg for every line that is an alignment
	 * and returns the last line that isn't an alignment
	 * (or null if iterator is empty)
	 */
	private def readAlignments(iter: Iterator[String], doc: RichConcreteDocBuilder, storeMentionsIn: Buffer[HalfAlignment[Argument]]): String = {
		var ret: String = null
		while(ret == null) {
			if(!iter.hasNext)
				return null
			val line = iter.next
			if(!addAPredArg(line, doc, storeMentionsIn))
				ret = line
		}
		ret
	}


	/**
	 * tries to find a word given only a paragraph and sentence idx
	 * returns (sentenceIdx, tokenIdx)
	 */
	private def findWord(word: String, doc: Document, paragraph: Int, occurrence: Int): (Int, Int) = {
		var seen = 0
		var r = (-1, -1)
		for((sent, sentenceIdx) <- doc.sentences.zipWithIndex.drop(paragraph)) {	// there is at least one sentence per paragraph
			for((tok, tokIdx) <- sent.tokens.zipWithIndex) {
				val w = tok.getWord
				if(w.contains(word)) {
					seen += 1
					if(occurrence+1 == seen) {
						if(r._1 >= 0 && r._2 >= 0) {
							warning("DATA IS AMBIGUOUS!")
							assert(false)
						}
						r = (sentenceIdx, tokIdx)
					}
				}
			}
		}
		if(r == (-1,-1)) {
			warning("doc id = " + doc.id)
			warning("doc = " + Describe.document(doc))
			warning("word = " + word)
			warning("paragraph = " + paragraph)
			warning("occurrence = " + occurrence)
			assert(false)
		}
		assert(r._1 >= 0 && r._2 >= 0)
		assert(r._1 < doc.sentences.size)
		assert(r._2 < doc.sentences(r._1).size)
		r
	}
	
	
	/**
	 * returns true if the line matches the format of an alignment line
	 */
	private def addAPredArg(line: String, doc: RichConcreteDocBuilder,
			storeMentionsIn: Buffer[HalfAlignment[Argument]]): Boolean = {
		
		val Pat = """(\d+) (\d+) (\S+) (\d+) (possible|sure|none)""".r
		line match {
			case Pat(para_s, occurrence_s, word, corefSet, conf) => {
				val (para, occurrence) = (para_s.toInt - 1, occurrence_s.toInt - 1)
				assert(occurrence >= 0)

				// this data will provide some preds/args that have 's at the end
				// which is usually split in annotated gigaword: strip it off
				// e.g. "country's" vs ("country" "'s")
				val wordToUse = 
					if(word.endsWith("'s"))
						word.substring(0, word.size - 2)
					else word

				// the Roth and Frank data indexes words by their paragraph
				// but annotated gigaword only provides sentence indices
				// this is a moderately-safe approximate matching method
				val (sentenceIdx, tokIdx) = findWord(wordToUse, doc, para, occurrence)
				
				// following the behavior in AnnotationAligner.alignMentionString
				// I will just make a new Mention
				
				// the reason I'm not using AnnotationAligner.alignMentionString here
				// is because that can align to the wrong mention if there is a word
				// repeated in a sentence
				// (note findWord has a similar problem...)
		
				// head token = start because in Roth and Frank data all arguments
				// and predicates are one word
				val (left, right, head) = (tokIdx, tokIdx+1, tokIdx)
				val mention = MentionBuilder.from(sentenceIdx, left, right, head)

				val w = doc.getHeadToken(mention).getWord
				val pOrA: Either[Predicate, Argument] = {
					val p = new Predicate(mention)
					//println("pred at " + p.location + ": " + Describe.mentionInContext(p.location, doc))
				//	if(conf != "none")
				//		doc.addPredicate(p)//, corefSet, checkForDups=false)
					Left(p)
				}
				if(conf != "none")
					storeMentionsIn += new HalfAlignment(pOrA, corefSet, conf == "sure")
				
				true
			}
			case _ => false
		}
	}

}


