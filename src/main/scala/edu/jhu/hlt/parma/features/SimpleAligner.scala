// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.parma.util.Describe
import scala.collection.JavaConversions._
import java.util.logging._
import redis.clients.jedis.Jedis

//RedisStuff, PPDBRuleIO, and RedisPPDBSetup are in RedisPPDB.scala

case class SpanAlignment(wordScores: Seq[(String, String, Double)], unaligned: Int)

class SimpleAligner extends RedisPPDB {
	
	type PPDBMap = Map[String, Seq[PPDBRule]]

	@transient
	lazy val jedis = new Jedis(RedisStuff.hostname, RedisStuff.port)

	def pipeRedis(keys: Iterable[String]): PPDBMap = {//: Map[String, Seq[PPDBRule]] = {
		jedis.select(0)
		val pipe = jedis.pipelined
		keys.foreach {w => if (ppdbKeys.in(w)) pipe.lrange(w, 0, 100)}
		val results = pipe.syncAndReturnAll.map{ res =>
			res.asInstanceOf[java.util.List[String]].map{ rule => PPDBRuleIO.str2rule(rule) }
		}.map{ entry => if (entry.isEmpty) None
						else Some((entry.head.lhs +"|"+ entry.head.rhs, entry))}.flatten
		results.toMap.withDefaultValue(Seq())
	}

	override def name = "SimpleAligner"

	def jointQueries(rs: Seq[String], ps: Seq[String]): Seq[String] = {
		rs.flatMap(r => ps.map(p => r +"|"+ p))
	}

// Base on lhs lookups instead of pair lookups
//
//
//	def queryRedis(key: String): Seq[PPDBRule] = {
//		if (ppdbKeys.in(key)) {
//			val maxRHSs = 100
//			val values = jedis.lrange(key, 0, maxRHSs)
//			values.map(ruleStr => PPDBRuleIO.str2rule(ruleStr))
//		}
//		else Seq()
//	}
//
//	def possibleAlignments(a: String, b: Seq[String], ppdbMap: PPDBMap): Seq[PPDBRule] = {
//		ppdbMap(a).filter(rule => b.contains(rule.rhs))
//		//queryRedis(a).filter(rule => b.contains(rule.rhs))
//	}
//
//	def alignWordToSpan(a: String, b: Seq[String], ppdbMap: PPDBMap): Option[PPDBRule] = {
//		lazy val pas = possibleAlignments(a,b, ppdbMap)
//		if (pas.isEmpty) None
//		else Some(pas.maxBy(_.score))
//	}
//
//	def alignSpanToSpan(a: Seq[String], b: Seq[String], ppdbMap: PPDBMap): SpanAlignment = {
//		val rules = a.flatMap{w => alignWordToSpan(w, b, ppdbMap)}
//		val alignment = a.zip(rules)
//		val wordScores = alignment.collect{case (w,rule) => (w, rule.rhs, rule.score)}
//		val unaligned = a.size - wordScores.size
//		new SpanAlignment(wordScores, unaligned)
//	}
//	
// unneccessary
//	def forwardWordToSpan(a: String, b: Seq[String], ppdb: PPDBMap): Option[PPDBRule] = {
//		val possibles = b.map{ w => 
//			val key = a +"|"+ w
//			ppdb(key)
//		}.flatten
//		if (possibles.isEmpty) None
//		else Some(possibles.maxBy(_.score))
//	}

	
	def forwardSpanToSpan(as: Seq[String], bs: Seq[String], ppdb: PPDBMap): SpanAlignment = {
		val wordScores = as.map{ a => 
			val possibles = bs.map{ b => val key = a +"|"+ b; ppdb(key)}.flatten
			if (possibles.isEmpty) None
			else {
				val alignment = possibles.maxBy(_.score)
				Some((alignment.lhs, alignment.rhs, alignment.score))
			}
		}.flatten
		val unaligned = as.size - wordScores.size
		new SpanAlignment(wordScores, unaligned)
	}

	def backwardSpanToSpan(as: Seq[String], bs: Seq[String], ppdb: PPDBMap): SpanAlignment = {
		val wordScores = bs.map{ b => 
			val possibles = as.map{ a => val key = a +"|"+ b; ppdb(key)}.flatten
			if (possibles.isEmpty) None
			else {
				val alignment = possibles.maxBy(_.score)
				Some((alignment.lhs, alignment.rhs, alignment.score))
			}
		}.flatten
		val unaligned = as.size - wordScores.size
		new SpanAlignment(wordScores, unaligned)
	}
	def scoreAlignment(alignment: SpanAlignment, penalty: Int): Double = {
		alignment.wordScores.map(_._3).sum + (penalty*alignment.unaligned)
	}

	def align(reportSpan: Seq[String], passageSpan: Seq[String], ppdb: PPDBMap): (SpanAlignment,SpanAlignment) = {
		val forward = forwardSpanToSpan(reportSpan, passageSpan, ppdb)
		val backward = backwardSpanToSpan(reportSpan, passageSpan, ppdb)
		(forward, backward)
	}

	def termSimilarity(reportSpan: Seq[String], passageSpan: Seq[String], penalty: Int, ppdb: PPDBMap): Double = {

		val (f,b) = align(reportSpan,passageSpan, ppdb)
		if (f.wordScores.isEmpty || b.wordScores.isEmpty) -100.0
		else {
			//term scores = span score / aligned terms
			val fScore = scoreAlignment(f, penalty) / (reportSpan.size - f.unaligned)
			val bScore = scoreAlignment(b, penalty) / (passageSpan.size - b.unaligned) 
			//term similarity = average of forward and backward alignment score
			(fScore + bScore) / -2.0
		}
	}



	private[this] val binarizer = new edu.jhu.hlt.parma.util.FixedWidthBinarizer(5, false, -64d, -8d)
	private[this] val debug = false


	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {

		val penalty = 10 //parameter penalizing unaligned words, should maybe be tuned
		
		val (rcm, pcm) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		
		val rs = report.getSentence(rcm).toString.toLowerCase.split(" ")	// report.getMentionTokens(rcm).map(_.getWord)
		val ps = passage.getSentence(pcm).toString.toLowerCase.split(" ")

		lazy val ppdbMap = pipeRedis(jointQueries(rs,ps))


		//for(cutoff <- List(5d, 12d, 25d)) {
		for(cutoff <- 2d to 7d by 0.2d map { x => x*x }) {

			val morePurePPDBMap = ppdbMap.mapValues(_.filter(_.score <= cutoff)).withDefaultValue(Seq[PPDBRule]())

			val sentenceScore = termSimilarity(rs, ps, penalty, morePurePPDBMap)

			val rspan = report.getMentionString(rcm).split(" ")
			val pspan = passage.getMentionString(pcm).split(" ")
			val mentionScore = termSimilarity(rspan, pspan, penalty, morePurePPDBMap)

			val rhead = Seq(report.getHeadString(rcm))
			val phead = Seq(passage.getHeadString(pcm))
			val headScore = termSimilarity(rhead, phead, penalty, morePurePPDBMap)

			if(debug) {
				println("report mention = " + Describe.mentionInContext(rcm, report))
				println("passage mention = " + Describe.mentionInContext(pcm, passage))
				println("sentence score = " + sentenceScore)
				println("mention score = " + mentionScore)
				println("head score = " + headScore)
			}

			featureIndexer.start(sv)
			featureIndexer.addStable("sts-sentence", sentenceScore, binarizer)
			featureIndexer.addStable("sts-mention", mentionScore, binarizer)
			featureIndexer.addStable("sts-head", headScore, binarizer)
			featureIndexer.commit
		}
	}

}




/* vim: set noet : */
