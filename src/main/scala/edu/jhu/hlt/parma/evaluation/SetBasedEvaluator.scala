// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.evaluation

import edu.jhu.hlt.parma.util.Describe
import edu.jhu.hlt.parma.inference.DocMetaAligner
import edu.jhu.hlt.parma.types._
import java.util.logging._

/**
 * note that generous versions of precision, recall, and F1
 * reduce to their regular counterparts if there are only sure alignments
 */
object SetBasedEvaluator {

	val log = Logger.getLogger(this.getClass.getName)

	private def checkInstance(inst: Instance[DocAlignment]) {
		assert(inst.hyp.exactlyPossibleAlignments.size == 0)
		assert(inst.hyp.possibleAlignments.size == inst.hyp.sureAlignments.size)
		assert(inst.gold.possibleAlignments.size >= inst.gold.sureAlignments.size)
	}

	def truePos(inst: Instance[DocAlignment]): Int =
		(inst.gold.possibleAlignments & inst.hyp.sureAlignments).size

	def trueNeg(inst: Instance[DocAlignment]): Int = {
		// this is mega inefficient, i think i really want that
		// "ExplicitDocAlignment" now. or possibly just push a "def notAligned: Seq[Alignment]"
		// method into DocAlignment, which naivey will be implemented as DocMetaAligner.allPossible - aligned
		val allGold = DocMetaAligner.allPossibleAlignments(inst.gold.report, inst.gold.passage).toSet
		val negGold = allGold -- inst.gold.possibleAlignments
		val allHyp = DocMetaAligner.allPossibleAlignments(inst.hyp.report, inst.hyp.passage).toSet
		val negHyp = allHyp -- inst.hyp.sureAlignments
		(negGold & negHyp).size
	}

	def falsePos(inst: Instance[DocAlignment]): Int = {
		val allGold = DocMetaAligner.allPossibleAlignments(inst.gold.report, inst.gold.passage).toSet
		val negGold = allGold -- inst.gold.possibleAlignments
		(negGold & inst.hyp.sureAlignments).size
	}

	def falseNeg(inst: Instance[DocAlignment]): Int = {
		val allHyp = DocMetaAligner.allPossibleAlignments(inst.hyp.report, inst.hyp.passage).toSet
		val negHyp = allHyp -- inst.hyp.sureAlignments
		(inst.gold.sureAlignments & negHyp).size
	}

	def hamming(inst: Instance[DocAlignment], falsePosPenalty: Double = 1d,
			falseNegPenalty: Double = 1d, normalize: Boolean = true): Double = {

		checkInstance(inst)
		val h = inst.hyp.sureAlignments
		val g = inst.gold.possibleAlignments
		val falsePos = (h -- g).size
		val falseNeg = (g -- h).size

		val unNormalized = falsePos * falsePosPenalty + falseNeg * falseNegPenalty
		
		if(normalize) {
			val rp = inst.gold.report.predicates.size
			val pp = inst.gold.passage.predicates.size
			val ra = inst.gold.report.corefs.size
			val pa = inst.gold.passage.corefs.size
			val numPossibleAlignments = (rp * pp) + (ra * pa)
			val maxFalsePos = numPossibleAlignments - g.size
			val maxFalseNeg = g.size
			val normalizer = maxFalsePos * falsePosPenalty + maxFalseNeg * falseNegPenalty
			unNormalized / normalizer
		}
		else unNormalized
	}

	/**
	 * predicate (only) normalized hamming loss
	 * NOTE: should only be used for debugging!
	 */
	def hammingPN(inst: Instance[DocAlignment], falsePosPenalty: Double = 1d, falseNegPenalty: Double = 1d): Double = {

		checkInstance(inst)
		val h = inst.hyp.surePredicateAlignments.toSet
		val g = inst.gold.possiblePredicateAlignments.toSet
		val falsePos = (h -- g).size
		val falseNeg = (g -- h).size

		val unNormalized = falsePos * falsePosPenalty + falseNeg * falseNegPenalty
		
		val rp = inst.gold.report.predicates.size
		val pp = inst.gold.passage.predicates.size
		val numPossibleAlignments = rp * pp
		val maxFalsePos = numPossibleAlignments - g.size
		val maxFalseNeg = g.size
		val normalizer = maxFalsePos * falsePosPenalty + maxFalseNeg * falseNegPenalty
		unNormalized / normalizer
	}

	// page 602 on http://aclweb.org/anthology-new/J/J08/J08-4005.pdf
	def generousPrecision(inst: Instance[DocAlignment],
			takePreds: Boolean = true, takeArgCorefs: Boolean = true): Double = {
		
		assert(takePreds || takeArgCorefs)
		checkInstance(inst)
		
		val (h, g): (Set[Alignment], Set[Alignment]) =
			if (takePreds && takeArgCorefs) (inst.hyp.sureAlignments, inst.gold.possibleAlignments)
			else if (takePreds) (inst.hyp.surePredicateAlignments.toSet, inst.gold.possiblePredicateAlignments.toSet)
			else if (takeArgCorefs) (inst.hyp.sureArgCorefAlignments.toSet, inst.gold.possibleArgCorefAlignments.toSet)
			else throw new RuntimeException
		if(h.size == 0) 1d
		else (h & g).size.toDouble / h.size
	}
	
	// page 602 on http://aclweb.org/anthology-new/J/J08/J08-4005.pdf
	def generousRecall(inst: Instance[DocAlignment],
			takePreds: Boolean = true, takeArgCorefs: Boolean = true): Double = {
		
		assert(takePreds || takeArgCorefs)
		checkInstance(inst)
		
		val (h, g): (Set[Alignment], Set[Alignment]) =
			if (takePreds && takeArgCorefs) (inst.hyp.possibleAlignments, inst.gold.sureAlignments)
			else if (takePreds) (inst.hyp.possiblePredicateAlignments.toSet, inst.gold.surePredicateAlignments.toSet)
			else if (takeArgCorefs) (inst.hyp.possibleArgCorefAlignments.toSet, inst.gold.sureArgCorefAlignments.toSet)
			else throw new RuntimeException
		if(g.size == 0) 1d
		else (h & g).size.toDouble / g.size
	}
	
	def generousF1(instance: Instance[DocAlignment],
			takePreds: Boolean = true, takeArgCorefs: Boolean = true): Double = {

		checkInstance(instance)
		val p = generousPrecision(instance, takePreds, takeArgCorefs)
		val r = generousRecall(instance, takePreds, takeArgCorefs)

		/*
		println("[generousF1] inst.gold.sure = " + instance.gold.sureAlignments.size)
		println("[generousF1] inst.gold.possible = " + instance.gold.possibleAlignments.size)
		println("[generousF1] inst.hyp.sure = " + instance.hyp.sureAlignments.size)
		println("[generousF1] inst.hyp.possible = " + instance.hyp.possibleAlignments.size)

		println("[generousF1] precision = " + p)
		println("[generousF1] recall = " + r)
		*/

		if(p + r == 0d) 0d
		else 2d*p*r / (p+r)
	}
	
	def microAvg[T](instances: Seq[Instance[T]], perf: Instance[T] => Double, weight: T => Double): Double = {
		assert(instances.size > 0)
		var num = 0d
		var denom = 0d
		instances.foreach(inst => {
			val w = weight(inst.gold)
			val p = perf(inst)
			num += w*p
			denom += w
		})
		num / denom
	}

	def macroAvg[T](instances: Seq[Instance[T]], perf: Instance[T] => Double): Double = {
		microAvg(instances, perf, (gold: T) => 1d)
	}
}

