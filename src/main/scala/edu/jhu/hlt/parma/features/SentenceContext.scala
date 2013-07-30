// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.parma.util.Misc

class SentenceContext extends AlignmentSimilarity {

	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {
		val (reportMention, passageMention) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val reportSent = report.getSentence(reportMention)
		val passageSent = passage.getSentence(passageMention)
		featureIndexer.start(sv)
		dependencyParseFeatures(reportMention, report, passageMention, passage)
		leftRightContextFeatures(reportMention, reportSent, passageMention, passageSent)
		featureIndexer.commit
	}
	
	// TODO {bag of words match} x {go up the constituency tree}
	
	/**
	 * {bag of words match} x {left, right context}
	 * 
	 * TODO get dist similarity to backoff on words
	 * (e.g. "ran" and "sprinted" are similar)
	 */
	def leftRightContextFeatures(
			reportMention: Mention, reportSent: Sentence,
			passageMention: Mention, passageSent: Sentence) {
	
		val r_left = reportSent.before(reportMention)
		val p_left = passageSent.before(passageMention)
		setFeatures("left-word", r_left.map(_.getWord).toSet, p_left.map(_.getWord).toSet)
		setFeatures("left-lemma", r_left.map(_.getLemma).toSet, p_left.map(_.getLemma).toSet)
		setFeatures("left-pos", r_left.map(_.getPosTag).toSet, p_left.map(_.getPosTag).toSet)
		setFeatures("left-ner", r_left.map(_.getNerTag).toSet, p_left.map(_.getNerTag).toSet)
		
		val r_right = reportSent.after(reportMention)
		val p_right = passageSent.after(passageMention)
		setFeatures("right-word", r_right.map(_.getWord).toSet, p_right.map(_.getWord).toSet)
		setFeatures("right-lemma", r_right.map(_.getLemma).toSet, p_right.map(_.getLemma).toSet)
		setFeatures("right-pos", r_right.map(_.getPosTag).toSet, p_right.map(_.getPosTag).toSet)
		setFeatures("right-ner", r_right.map(_.getNerTag).toSet, p_right.map(_.getNerTag).toSet)
	}
	
	/**
	 * {typed, untyped} x {governer, dependent} x {dice, special-dice}
	 */
	def dependencyParseFeatures(
			reportMention: Mention, report: Document,
			passageMention: Mention, passage: Document) {
	  
		val rsent = report.getSentence(reportMention)
		val psent = passage.getSentence(passageMention)
		
		// do these two mentions govern the same things?
		val r_dep = rsent.governedBy(reportMention)
		val p_dep = psent.governedBy(passageMention)		
		setFeaturesHelper((t: Token) => t.getLemma, "dep-lemma", r_dep, p_dep)
		setFeaturesHelper((t: Token) => t.getPosTag, "dep-pos", r_dep, p_dep)
		setFeaturesHelper((t: Token) => t.getNerTag, "dep-ner", r_dep, p_dep)

		// do these two mentions depend on the same things?
		val r_gov = rsent.governs(reportMention)
		val p_gov = psent.governs(passageMention)		
		setFeaturesHelper((t: Token) => t.getLemma, "gov-lemma", r_gov, p_gov)
		setFeaturesHelper((t: Token) => t.getPosTag, "gov-pos", r_gov, p_gov)
		setFeaturesHelper((t: Token) => t.getNerTag, "gov-ner", r_gov, p_gov)
	}
	
	private def setFeaturesHelper[T](f: Token => T, tag: String,
	    report_deps: Seq[Dependency[Token]], passage_deps: Seq[Dependency[Token]]) {
	  
		// preserve dependency type
		val r_dep = report_deps.map(d => d.map(f)).toSet
		val p_dep = passage_deps.map(d => d.map(f)).toSet
		setFeatures(tag, r_dep, p_dep)

		// ignore dependency type
		val r_dep_untyped = report_deps.map(d => d.map(f)).map(_.toUntyped).toSet
		val p_dep_untyped = passage_deps.map(d => d.map(f)).map(_.toUntyped).toSet
		setFeatures(tag+"-untyped", r_dep_untyped, p_dep_untyped)
	}
	
	private def setFeatures[T](tag: String, rset: Set[T], pset: Set[T]) {
		val intersect = (rset & pset).size
		featureIndexer.addStable(tag+"-intersect0", bool2value(intersect == 0 && rset.size > 0 && pset.size > 0))
		featureIndexer.addStable(tag+"-intersect1", bool2value(intersect == 1))
		featureIndexer.addStable(tag+"-intersect2", bool2value(intersect >= 2))
		featureIndexer.addStable(tag+"-intersect3", bool2value(intersect == rset.size && rset.size == pset.size))
	}
}



