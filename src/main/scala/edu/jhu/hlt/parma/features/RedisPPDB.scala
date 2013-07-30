// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.feature_interfaces.AlignmentSimilarity
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.parma.util.SHA1er
import java.security.MessageDigest
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import redis.clients.jedis._

object RedisStuff {
	val hostname = ParmaConfig.getString("features.ppdb.redis.host")
	val port = ParmaConfig.getInt("features.ppdb.redis.port")

	override def toString: String = "(RedisStuff hostname=%s port=%d)".format(hostname, port)
	
	/**
	 * redis likes you to give keys that are already hashed, more efficient
	 */
	def makeKey(parts: String*): String = parts.mkString(":")
}

// client
class RedisPPDB extends AlignmentSimilarity {
	import edu.jhu.hlt.parma.features.RedisBloomFilter
	
	@transient
	private var redis: Jedis = null
	
	override def name = "RedisPPDB"
	
	override def setup(calibrateOn: java.util.Collection[DocAlignment]) {
		redis = new Jedis(RedisStuff.hostname, RedisStuff.port)
		if(redis.ping != "PONG")
			throw new RuntimeException("cannot connect to redis! " + RedisStuff.toString)
		redis.select(1)
	}


	private[this] val binarizer = new FixedWidthBinarizer(8, false, -22d, -2d)
	private[this] val emptyFeatures = DVec.rep(0d/*binarizer.agnostic*/, 7)
	private def featuresFromRule(rule: PPDBRule): DVec = {
		implicit def ppdbweight2double(s: String): Double = {
			val v = java.lang.Double.parseDouble(s)
			assert(v >= 0d)
			math.exp(-v)
		}
		val dv = new DVec(
			rule.properties("p(LHS|f)"),
			rule.properties("p(e|LHS)"),
			rule.properties("p(e|f)"),
			rule.properties("p(e|f,LHS)"),
			rule.properties("p(f|LHS)"),
			rule.properties("p(f|e)"),
			rule.properties("p(f|e,LHS)")
		)
		assert(dv.dimension == emptyFeatures.dimension)
		dv
	}

	var q = 0
	var qAtLeastOneHit = 0
	var qAtLeastTwoHits = 0
	val qStep = 5000
	@transient
	lazy val ppdbKeys = RedisBloomFilter.initialize(true)
	private def queryRedis(a: String, b: String, featureName: String, aPOS: String, bPOS: String) {
		val start = System.currentTimeMillis
		val key = a// RedisStuff.makeKey(a, b)
		val maxRHSs = 1000
		val values = if (!ppdbKeys.in(key)) new java.util.ArrayList()
								 else redis.lrange(key, 0, maxRHSs)
		var qi = 0
		val hits = new ArrayBuffer[PPDBRule]
		val hitsPOS = new ArrayBuffer[PPDBRule]
		var sumFeatures: DVec = null
		var maxFeatures: DVec = null
		values.foreach(ruleStr => {
			val rule = PPDBRuleIO.str2rule(ruleStr)
			if(rule.rhs == b) {
				if(q % qStep == 0)
					println("match! q=%d, a=%s, b=%s, rule=%s".format(q, a, b, rule))
				val features = featuresFromRule(rule)
				if(sumFeatures == null) {
					sumFeatures = features.copy
					maxFeatures = features.copy
				} else {
					sumFeatures += features
					maxFeatures.maxEquals(features)
				}
				hits += rule
				if(rule.parent == aPOS && aPOS == bPOS)
					hitsPOS += rule
			}
		})
		if(sumFeatures == null) sumFeatures = emptyFeatures
		for(i <- 0 until sumFeatures.dimension)
			featureIndexer.addUnstable("hits-sum"+i.toString, sumFeatures(i))
		if(maxFeatures == null) maxFeatures = emptyFeatures
		for(i <- 0 until maxFeatures.dimension)
			featureIndexer.addUnstable("hits-max"+i.toString, maxFeatures(i))

		featureIndexer.addUnstable("numHits", hits.size.toDouble / 10d)
		featureIndexer.addUnstable("numHits1", bool2value(hits.size >= 1))
		featureIndexer.addUnstable("numHits2", bool2value(hits.size >= 2))
		featureIndexer.addUnstable("numHits3", bool2value(hits.size >= 4))
		featureIndexer.addUnstable("numHits4", bool2value(hits.size >= 8))
		featureIndexer.addUnstable("numHits5", bool2value(hits.size >= 16))
		featureIndexer.addUnstable("numHits6", bool2value(hits.size >= 32))
		featureIndexer.addUnstable("numHits7", bool2value(hits.size >= 64))


		// filter out PPDB matches that don't match the POS in these mentions
		featureIndexer.addUnstable("numHits-POS", hitsPOS.size.toDouble / 5d)
		featureIndexer.addUnstable("numHits-POS1", bool2value(hitsPOS.size >= 1))
		featureIndexer.addUnstable("numHits-POS2", bool2value(hitsPOS.size >= 2))
		featureIndexer.addUnstable("numHits-POS3", bool2value(hitsPOS.size >= 4))


		// filter based on PPDB score, count how many hits are left
		for(cutoff <- 2d to 6d by 0.2d map { x => x*x }) {
			val c = hits.filter(_.score < cutoff).size
			val cp = hitsPOS.filter(_.score < cutoff).size
			featureIndexer.addUnstable("hits<%.1f".format(cutoff), c / 2d)
			featureIndexer.addUnstable("hits<%.1f-POS".format(cutoff), cp.toDouble)
		}


		val (f1dv, f2dv) =
			if(hits.size > 0) {
				qAtLeastOneHit += 1
				if(hits.size > 1)
					qAtLeastTwoHits += 1
				val l = hits.minBy(_.properties("p(e|f)").toDouble)
				val r = hits.minBy(_.properties("p(f|e)").toDouble)
				val ldv = featuresFromRule(l)
				val rdv = featuresFromRule(r)
				(DVec.max(ldv, rdv), DVec.sum(ldv, rdv))
			}
			else (emptyFeatures, emptyFeatures)
		for(i <- 0 until f1dv.dimension)
			featureIndexer.addUnstable("p(e|f)"+i.toString, f1dv(i))
		for(i <- 0 until f2dv.dimension)
			featureIndexer.addUnstable("p(f|e)"+i.toString, f2dv(i))


		if(q % qStep == 0) {
			val free = Runtime.getRuntime.freeMemory / 1024f / 1024f
			val maxmem = Runtime.getRuntime.maxMemory / 1024f / 1024f
			println("[RedisPPDB featurize] free=%.1f MB, maxMem=%.1f MB".format(free, maxmem))
			println("[RedisPPDB featurize] q=%d qAtLeastOneHit=%d, qAtLeastTwoHits=%d, a=%s, b=%s, %d values processed in %d ms"
				.format(q, qAtLeastOneHit, qAtLeastTwoHits, a, b, values.size, System.currentTimeMillis - start))
		}
		q += 1
	}

	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {

		featureIndexer.start(sv)

		val (rCM, pCM) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val rsent = report.getSentence(rCM)
		val psent = passage.getSentence(pCM)

		def pos(t: Token) = "[" + t.getPosTag.toLowerCase + "]"
		def word(t: Token) = t.getWord.toLowerCase

		// [1] mention head word
		val rw = report.getHeadToken(rCM)//.getWord.toLowerCase
		val pw = passage.getHeadToken(pCM)//.getWord.toLowerCase
		if(rw != pw) {
			queryRedis(word(rw), word(pw), "headTok", pos(rw), pos(pw))
			queryRedis(word(pw), word(rw), "headTok", pos(pw), pos(rw))
		}

		// [2] mention head lemma
		//val rMention = report.getMentionSpan(rCM).map(_.getLemma).mkString(" ")
		//val pMention = passage.getMentionSpan(pCM).map(_.getLemma).mkString(" ")
		//queryRedis(rMention, pMention, "mention", addTo)
		//queryRedis(pMention, rMention, "mention", addTo)

		/*
		// [3] 1..k word phrases to the left of both mentions
		val rLeft = rsent.before(rCM)
		val pLeft = psent.before(pCM)
		for(i <- 1 to 3) {
			val rls = rLeft.takeRight(i).map(_.getWord).mkString(" ")
			val pls = pLeft.takeRight(i).map(_.getWord).mkString(" ")
			queryRedis(rls, pls, "left"+i, addTo)
		}

		// [4] 1..k word phrases to the right of both mentions
		val rRight = rsent.after(rCM)
		val pRight = psent.after(pCM)
		for(i <- 1 to 3) {
			val rrs = rRight.take(i).map(_.getWord).mkString(" ")
			val prs = pRight.take(i).map(_.getWord).mkString(" ")
			queryRedis(rrs, prs, "right"+i, addTo)
		}

		// [5] 1..k word phrases on both sides of both mentions
		for(i <- 1 to 3) {
			val r = rsent.slice(rCM.getStartTokenIdx - i, rCM.getEndTokenIdx + i).map(_.getWord).mkString(" ")
			val p = psent.slice(pCM.getStartTokenIdx - i, pCM.getEndTokenIdx + i).map(_.getWord).mkString(" ")
			queryRedis(r, p, "overlap"+i, addTo)
		}
		*/
		featureIndexer.commit
	}

	
}

object PPDBRuleIO {
	import scala.collection.mutable.HashMap

	// TODO fix this, only tab is supported now
	val sep1 = "\t"
	val sep2 = "\t"
	val eq = "\t"

	def str2rule(redisStr: String): PPDBRule = {
		val ar = redisStr.split("\t")
		val lhs = ar(0)
		val rhs = ar(1)
		val parent = ar(2)
		val score = ar(3).toDouble
		val props = new HashMap[String, String]
		assert(ar.length % 2 == 0)
		for(i <- 4 until ar.length by 2)
			props.update(ar(i), ar(i+1))
		new PPDBRule(lhs, rhs, parent, score, props)
	}

	def validToken(s: String) = s.indexOf(sep1) < 0 && s.indexOf(sep2) < 0 && s.indexOf(eq) < 0

	private def sortedKVs(rule: PPDBRule): String = rule.properties.map(kv => kv._1 + eq + kv._2).toBuffer.sorted.mkString(sep2)

	def rule2str(rule: PPDBRule): String = {
		if(!validToken(rule.lhs))
			println("bad rule.lhs: " + rule.lhs)
		if(!validToken(rule.rhs))
			println("bad rule.rhs: " + rule.rhs)
		assert(validToken(rule.lhs))
		assert(validToken(rule.rhs))
		assert(validToken(rule.parent))
		rule.properties.foreach(kv => {
			if(!validToken(kv._1) || !validToken(kv._2))
				println("bad: " + kv)
			assert(validToken(kv._1))
			assert(validToken(kv._2))
		})
		List(rule.lhs, rule.rhs, rule.parent, rule.score.toString, sortedKVs(rule)).mkString(sep1)
	}
}

class PPDBRule(val lhs: String, val rhs: String, val parent: String, val score: Double, val allProperties: scala.collection.Map[String, String]) {

	val neededKeys = Set("p(LHS|e)", "p(LHS|f)", "p(e|LHS)", "p(e|f)", "p(e|f,LHS)", "p(f|LHS)", "p(f|e)", "p(f|e,LHS)")
	val properties = allProperties.filterKeys(k => neededKeys.contains(k))

	lazy val logProb = neededKeys.map{ k => -allProperties(k).toDouble }.sum
	override def toString = PPDBRuleIO.rule2str(this)
}


// code needed to insert into redis
object RedisPPDBSetup {
	val ppdbKeys = RedisBloomFilter.initialize(true)

	def main(args: Array[String]) {
		if(args.length != 1) {
			println("please provide a parma.config file")
			sys.exit(-1)
		}
		ParmaConfig.load(args(0))
		println("[redis insert] trying to connect to redis at %s on port %d".format(RedisStuff.hostname, RedisStuff.port))
		val redis = new Jedis(RedisStuff.hostname, RedisStuff.port)
		// load redis db 1 instead of db 0
		redis.select(1)
		val f = ParmaConfig.getFile("features.ppdb.redis.file.lexical")
		val reader = FileUtils.getReader(f)

		//println("[redis insert] flushing existing stuff...")
		//val t = redis.multi
		//t.flushDB
		//t.exec
		println("for some reason, cannot flush programatically, do so manually")

		println("[redis insert] inserting stuff...")
		//val pipe = redis.pipelined
		var i = 0
		val start = System.currentTimeMillis
		var last = start
		while(reader.ready) {
			val line = reader.readLine
			val ar = line.split(" \\|\\|\\| ")
			val score = ar(0).toDouble
			val parent = ar(1).toLowerCase
			val leftChild = ar(2).toLowerCase
			val rightChild = ar(3).toLowerCase
			val properties = str2map(ar(ar.length-2))

			val key = leftChild //+"|"+ rightChild
			val value = PPDBRuleIO.rule2str(new PPDBRule(leftChild, rightChild, parent, score, properties))
			//val value = List(rightChild, properties("p(LHS|e)"), properties("p(LHS|f)"),
			//				properties("p(e|LHS)"), properties("p(e|f)"), properties("p(e|f,LHS)"),
			//				properties("p(f|LHS)"), properties("p(f|e)"), properties("p(f|e,LHS)"))
			//println("key = %s, value = [%s]".format(key, value))
			//redis.rpush(key, value: _*)
			redis.rpush(key, value)
			ppdbKeys.set(key)
			//pipe.rpush(key, value)

			val step = 15000
			if(i % step == 0) {
				val now = System.currentTimeMillis
				val recent = step.toDouble / (now-last)
				val avg = i.toDouble / (now-start)
				println("i=%d, %.1f K lines/sec recent, %.1f K lines/sec avg".format(i, recent, avg))
				//println("adding(%d) %s => %s (%.1f %.1f K lines/sec recent/avg)".format(i, key, properties, recent, avg))
				last = now
			}
			i += 1
		}
		reader.close
		ppdbKeys.write
		//pipe.exec

		println("done, added %d rules in %.1f minutes".format(i, (System.currentTimeMillis - start)/(60d*1000d)))
	}

	private def str2map(kvs: String): Map[String, String] = {
		val properties = kvs.split(" ").flatMap(kv => {
			val x = kv.split("=")
			if(x.length != 2) {
				//println("kvs = " + kvs)
				//throw new RuntimeException("x = [%s]".format(x.mkString(", ")))
				Seq()
			}
			else Seq((x(0), x(1)))
		}).toMap

		// get rid of trailing 0s
		properties.mapValues(value => """\.0*$""".r.replaceAllIn(value, ""))
	}

}



/* vim: set noet : */
