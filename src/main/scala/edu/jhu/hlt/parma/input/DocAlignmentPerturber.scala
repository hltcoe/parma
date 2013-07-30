// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.{ DocMetaAligner, CanonicalMentionFinder }
import edu.jhu.hlt.parma.util.Describe
import edu.jhu.hlt.parma.diagnostics.GeneralDiagnostics
import scala.util.Random
import scala.collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer, HashSet}

/**
 * the purpose of this code is to make negative examples for pred-arg alignment
 */
object DocAlignmentPerturber {

	private def randomElem[T](iseq: IndexedSeq[T]): T = iseq(Random.nextInt(iseq.size))

	val docsSeen = new HashSet[Document]

	sealed class SubIn(val sentence: Sentence,
					val preds: Seq[Predicate],		// these have corefSets begining with "frankenstein"
					val args: Seq[Argument])		// these have corefSets begining with "frankenstein"

	private def chooseIncoming(howManySentences: Int, source: Seq[Document]): Seq[SubIn] = {
		// choose one sentence per document
		val subIns = new ArrayBuffer[SubIn]
		for(d <- Random.shuffle(source.toBuffer).take(howManySentences)) {
			val sentIdx = Random.nextInt(d.sentences.size)
			val sent: Sentence = d.sentences(sentIdx)
			val preds = d.predicates.filter(_.location.getSentenceIdx == sentIdx)
			val args = d.arguments.filter(_.location.getSentenceIdx == sentIdx)
			val anonPreds = preds.map(p => new Predicate(p.location))//, "frankenstein"+p.getCorefSet))
			val anonArgs = args.map(a => new Argument(a.location))//, "frankenstein"+a.getCorefSet))
			subIns += new SubIn(sent, anonPreds, anonArgs)
		}
		subIns
	}


	private def changeMentionSentence(newSentIdx: Int, m: Mention, isRepresentative: Boolean = false): Mention =
		MentionBuilder.from(newSentIdx, m.getStartTokenIdx, m.getEndTokenIdx, m.getHeadTokenIdx)

	private def modifyDocument(doc: DocumentBuilder, targetSentences: Seq[Int], substitutions: Seq[SubIn], propRemove: Double): DocumentBuilder = {
		assert(targetSentences.size == substitutions.size)

		val newDoc = doc.deepCopy

		// remove preds/args in targetSentences
		val predsKeep = doc.predicates.filterNot(pred => targetSentences.contains(pred.location.getSentenceIdx))
		val argsKeep = doc.arguments.filterNot(arg => targetSentences.contains(arg.location.getSentenceIdx))
		for(pred <- predsKeep)
			newDoc.addPredicate(pred)//, doc.corefSet(pred))
		for(arg <- argsKeep)
			newDoc.addArgument(arg)//, doc.corefSet(arg))

		// add preds/args from subIns
		// swap in Sentences from subIns
		for((targetIdx, subIn) <- targetSentences.zip(substitutions)) {

			newDoc.setSentence(targetIdx, subIn.sentence)

			// need to make sure all these preds/args have locations which point to sentenceIdx=targetIdx
			if(Random.nextDouble > propRemove) {

				// move all of the predicates and arguments to the new document
				for(pred <- subIn.preds) {
					val movedPred = new Predicate(changeMentionSentence(targetIdx, pred.location))
					//val corefSet = doc.corefSet(pred)
					newDoc.addPredicate(movedPred)//, corefSet)
				}
				for(arg <- subIn.args) {
					val movedArg = new Argument(changeMentionSentence(targetIdx, arg.location))
					//val corefSet = doc.corefSet(arg)
					newDoc.addArgument(movedArg)//, corefSet)
				}

			}
		}

		// for testing
		for(s <- substitutions) {
			for(p <- s.preds) println("[modifyDocument] sub.pred=" + Describe.predicate(p, newDoc))
			for(a <- s.args) println("[modifyDocument] sub.arg=" + Describe.argument(a, newDoc))
		}
		for(arg <- argsKeep) println("[modifyDocument] argsKeep=" + Describe.argument(arg, newDoc))
		for(pred <- predsKeep) println("[modifyDocument] predsKeep=" + Describe.predicate(pred, newDoc))
		for(arg <- newDoc.arguments) println("[modifyDocument] arg=" + Describe.argument(arg, newDoc))
		for(pred <- newDoc.predicates) println("[modifyDocument] pred=" + Describe.predicate(pred, newDoc))
		assert(newDoc.predicates.size > 0)
		assert(newDoc.arguments.size > 0)

		newDoc
	}

	private def modifyAlignment[D <: DocumentBuilder](da: ParametricDocAlignment[D], newPassage: D, targetSentences: Seq[Int]): ParametricDocAlignment[D] = {
		val zombieSents = targetSentences.toSet
		def inZombieSentence(a: Alignment): Boolean = a match {
			case pa: PredicateAlignment =>
				zombieSents.contains(pa.passagePred.location.getSentenceIdx)
			case aca: ArgCorefAlignment =>
				val ps = aca.passageCoref.map(_.location.getSentenceIdx).toSet
				(zombieSents & ps).size > 0
		}
		val skeep = da.sureAlignments.filterNot(inZombieSentence)
		val pkeep = da.possibleAlignments.filterNot(inZombieSentence)
		//println("[modifyAlignment] da.sure %d => %d, da.possible %d => %d"
		//	.format(da.sureAlignments.size, skeep.size, da.possibleAlignments.size, pkeep.size))
		val newDA = new ParametricDocAlignment[D](da.id, da.domain, da.report, newPassage, skeep, pkeep)
		GeneralDiagnostics.checkDocAlignment(newDA)
		//println("[modifyAlignment] success!")

		//for(a <- newDA.sureAlignments)
		//	println("[modifyAlignment] sure=" + Describe.alignment(a, newDA.report, newDA.passage))
		//for(a <- newDA.possibleAlignments)
		//	println("[modifyAlignment] possible=" + Describe.alignment(a, newDA.report, newDA.passage))

		/*
		assert(newDA.report.predicates.size > 0)
		assert(newDA.report.arguments.size > 0)
		assert(newDA.passage.predicates.size > 0)
		assert(newDA.passage.arguments.size > 0)
		*/

		newDA
	}

	/**
	 * propSub of the sentences in passage will be swapped out for a random sentence
	 * propRemove of the swapped sentences will have their Arguments and Predicates dropped
	 * (fewer Arguments/Predicates makes for faster inference)
	 */
	def degradeDocAlignment(
				da: ParametricDocAlignment[DocumentBuilder],
				outOfDomain: Seq[Document],
				propSub: Double = 0.6,
				propRemove: Double = 0.75): ParametricDocAlignment[DocumentBuilder] = {
		val n = da.passage.sentences.size
		val howManySents = math.ceil(n * propSub).toInt
		val subIns = chooseIncoming(howManySents, outOfDomain)
		val targetSentences = Random.shuffle((0 until n).toBuffer).take(howManySents)
		val newPassage = modifyDocument(da.passage, targetSentences, subIns, propRemove)
		val newDA = modifyAlignment[DocumentBuilder](da, newPassage, targetSentences)

		// i don't think that the way of aligning here works once corefSet
		// is removed from Document. need to use AnnotationAligner.HalfAlignment
		throw new RuntimeException("you need to update this code")

		newDA
	}





	// TODO everything below this <<<<<<<<<<<<<<<<<<<<<<<<<<<
	// should be moved to another class
	// below this is used to select a subset, not perturb a doc alignment


	def lowOverlap[DA <: DocAlignment](alignments: Seq[DA], maxOverlappiness: Double): Seq[DA] = {
		val fewer = alignments.filter(da => overlappiness(da) <= maxOverlappiness)
		println("[lowOverlap] thresh=%.1f, went from %d to %d alignments".format(maxOverlappiness, alignments.size, fewer.size))
		fewer
	}

	
	def leastOverlapSubset[DA <: DocAlignment](alignments: Seq[DA], howMany: Int): Seq[DA] = {
		val sorted = alignments.sortBy(overlappiness)
		
		val k = 20
		println("[DocAlignmentPerturber leastOverlapSubset] taking %d of %d doc alignments".format(howMany, alignments.size))
		println("[DocAlignmentPerturber leastOverlapSubset] least-overlappy %d alignments:".format(k))
		for((da, idx) <- sorted.take(k).zipWithIndex)
			println("[DocAlignmentPerturber leastOverlapSubset] overlappy=%.3g, da(%d)=%s".format(overlappiness(da), idx, Describe.docAlignment(da)))
		println("[DocAlignmentPerturber leastOverlapSubset] most-overlappy %d alignments:".format(k))
		for((da, idx) <- sorted.reverse.take(k).zipWithIndex)
			println("[DocAlignmentPerturber leastOverlapSubset] overlappy=%.3g, da(%d)=%s".format(overlappiness(da), idx, Describe.docAlignment(da)))
			
		val as = sorted.take(howMany)
		println("[DocAlignmentPerturber leastOverlapSubset] overlappiness of keep: " + as.map(overlappiness).mkString(", "))
		println("[DocAlignmentPerturber leastOverlapSubset] overlappiness of everything: " + sorted.map(overlappiness).mkString(", "))
		
		as
	}
		
	def leastOverlapSubset[DA <: DocAlignment](alignments: Seq[DA], propKeep: Double): Seq[DA] =
		leastOverlapSubset(alignments, math.ceil(alignments.size * propKeep).toInt)
	
	var counter = 0
	def overlappiness(da: DocAlignment): Double = {
		val sure = da.sureAlignments.toBuffer			// don't use set.map because it will collapse repeats
		val possible = da.possibleAlignments.toBuffer
		val s = sure.map(a => overlappiness(a, da.context)).sum
		val p = possible.map(a => overlappiness(a, da.context)).sum
		val eps = 1e-6d	// damn you floating point ops!
		if(p+eps < s) {
			println("p = " + p)
			println("s = " + s)
			println("sure.size = "+ sure.size)
			println("possible.size = " + possible.size)
			println("da.sureAlignments.size = " + da.sureAlignments.size)
			println("da.possibleAlignments.size = " + da.possibleAlignments.size)
		}
		assert(p+eps >= s)
		assert(da.sureAlignments.size <= da.possibleAlignments.size)
		assert(sure.size <= possible.size)
		val discount = 3d
		val num   = s         + (p             - s        ) / discount
		val denom = sure.size + (possible.size - sure.size) / discount
		//val num = s + (p-s) / discount
		//val denom = DocMetaAligner.allPossibleAlignments(da.report, da.passage).size.toDouble

		if(counter % 100 == 0) {
			println("[overlappiness] da=%s sure.size=%d possible.size=%d s=%.3g p=%.3g overlappy=%.8f"
				.format(da.id, da.sureAlignments.size, da.possibleAlignments.size, s, p, num/denom))
		}
		counter += 1

		if(denom <= 3)
			println("[overlappiness] WARNING: denom=%.1f for da.id=%s".format(denom, da.id))
		assert(denom > 0d)

		num / denom
	}
	
	def overlappiness(a: Alignment, c: Context): Double = {
		val (rCM, pCM) = CanonicalMentionFinder.canonicalMentions(a, c)
		val rt = c.report.getHeadToken(rCM)
		val pt = c.passage.getHeadToken(pCM)
		val lemma = if(rt.getLemma equalsIgnoreCase pt.getLemma) 1d else 0d
		val word = if(rt.getWord equalsIgnoreCase pt.getWord) 1d else 0d
		val pos = if(rt.getPosTag equalsIgnoreCase pt.getPosTag) 1d else 0d
		lemma + 0.1d*word + 0.05d*pos
	}
	
}

