// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.diagnostics

import java.util.logging.Logger
import edu.jhu.hlt.parma.features.FeatureLoader
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.experiments._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.evaluation.Instance

object GeneralDiagnostics {

	val log = Logger.getLogger(this.getClass.getName)
	val PRED_OUTFILE = "diagnostics.predictions.all"
	
    def checkConfig {
		assert(ParmaConfig.getStrings(ParmaConfig.EXPERIMENTS).size > 0)
		//val nf = FeatureLoader.getFeatures.size
		//println("FeatureLoader yielded %d features".format(nf))
		//assert(nf > 0)
	}

	def checkCorpus(corpus: DocAlignmentCorpus[_<:DocAlignment]) {
		println("checking corpus...")
		if(corpus.train.size == 0)
			println("no training examples!")
		if(corpus.dev.size == 0)
			println("no dev examples!")
		if(corpus.test.size == 0)
			println("no test examples!")
		for(da <- corpus.trainAlignments.iterator ++ corpus.testAlignments.iterator) {
			// Roth and Frank data contains some empty alignments, which we should keep,
			// but in general the alignments should be none-empty
			// if they are empty there is most likely a bug to be found
			// dev/pair_6 is an exception (empty alignment)
			var exception = false
			exception |= da.id.contains("dev.pair6")	// no alignments
			exception |= da.id.contains("dev.pair4")	// only possible alignments
			if(da.possibleAlignments.size == 0 && !exception)
				log.warning(da.id + " has no alignments!")
			assert(da.possibleAlignments.size >= da.sureAlignments.size)
			checkDocAlignment(da)
		}
		println("corpus looks OK!")
	}

	def checkDocument(doc: Document) {
		// predicates and args should not have duplicates
		val allMentions = (doc.predicates ++ doc.arguments).map(_.location)
		assert(allMentions.size == allMentions.toSet.size)
	}

	def checkDocAlignment(da: DocAlignment) {
		checkDocument(da.report)
		checkDocument(da.passage)
		for (pa <- da.possibleAlignments) {
			pa match {
				case p: PredicateAlignment => {
					checkMention(p.reportPred.location, da.report)
					checkMention(p.passagePred.location, da.passage)
				}
				case aa: ArgCorefAlignment => {
					for (a <- aa.reportCoref)
						checkMention(a.location, da.report)
					for (a <- aa.passageCoref)
						checkMention(a.location, da.passage)
				}
			}
		}
	}

	/**
	 * checks that this mention has valid indices in this doc
	 */
	def checkMention(mention: Mention, doc: Document) {
		assert(mention.getSentenceIdx >= 0)
		assert(mention.getSentenceIdx < doc.sentences.size)
		val s = doc.getSentence(mention)
		assert(mention.getStartTokenIdx >= 0)
		assert(mention.getEndTokenIdx <= s.tokens.size)
		assert(mention.getStartTokenIdx < mention.getEndTokenIdx)

		assert(mention.getStartTokenIdx < mention.getEndTokenIdx)
		assert(mention.getHeadTokenIdx >= mention.getStartTokenIdx)
		assert(mention.getHeadTokenIdx < mention.getEndTokenIdx)
	}

	def printDocAlignmentStatistics(alignments: Seq[DocAlignment], description: String) {
		val paa = (da: DocAlignment) => da.possibleAlignments.size
		val pa = (da: DocAlignment) => da.possiblePredicateAlignments.size
		val aa = (da: DocAlignment) => da.possibleArgCorefAlignments.size
		val stats = (desc: String, alignments: Seq[DocAlignment]) => {
			println("[corpusStatistics] in %s there are %d doc alignments".format(desc, alignments.size))
			println("[corpusStatistics] in %s there are %d pred/arg alignments".format(desc, alignments.map(paa).sum))
			println("[corpusStatistics] in %s there are %d predicate alignments".format(desc, alignments.map(pa).sum))
			println("[corpusStatistics] in %s there are %d argCoref alignments".format(desc, alignments.map(aa).sum))
			println("[corpusStatistics] in %s domains = %s".format(desc, alignments.groupBy(_.domain).mapValues(_.size)))
		}
		stats(description, alignments)
	}
  
	def printCorpusStatistics(corpus: DocAlignmentCorpus[DocAlignment]) {
		CanonicalMentionDiagnostics.printCanonicalMentions(corpus)
		printDocAlignmentStatistics(corpus.trainAlignments, corpus.id + "_train")
		printDocAlignmentStatistics(corpus.devAlignments, corpus.id + "_dev")
		printDocAlignmentStatistics(corpus.testAlignments, corpus.id + "_test ")
	}
	
	def outputPredictions(instances: Traversable[Instance[DocAlignment]]) {
		val filename = ParmaConfig.getString(PRED_OUTFILE, null)
		if(filename == null) return
		println("writing predictions")
		Profiler.startTask("outputPredictions")
		val out = new java.io.FileWriter(filename)   
		for(inst <- instances) {
			out.write(("="*40) + "\n")
			out.write(Describe.docAlignmentInstance(inst))
			out.write("\n\n")
		}
		out.close
		Profiler.endTask("outputPredictions")
  }
}


