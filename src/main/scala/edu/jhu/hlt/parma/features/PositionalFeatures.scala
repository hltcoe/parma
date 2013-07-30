// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import java.util.logging._

class PositionalFeatures extends AlignmentSimilarity {

	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {
		
		featureIndexer.start(sv)

		val (reportMention, passageMention) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val rs = reportMention.getSentenceIdx
		val ps = passageMention.getSentenceIdx
		featureIndexer.addStable("same-sentence", bool2value(rs == ps))
		featureIndexer.addStable("sent-diff", math.abs(rs - ps)/2d)
		featureIndexer.addStable("min-sentence", math.min(rs, ps))
		featureIndexer.addStable("max-sentence", math.max(rs, ps))
		featureIndexer.addStable("report-sentence", rs)
		featureIndexer.addStable("passage-sentence", ps)

		val rN = report.sentences.size
		val pN = passage.sentences.size
		featureIndexer.addStable("passage-doc-half", (2*rs)/rN)
		featureIndexer.addStable("report-doc-half", (2*ps)/pN)
		featureIndexer.addStable("same-doc-half", bool2value((2*rs)/rN == (2*ps)/pN))

		featureIndexer.addStable("passage-doc-quarter", (4*rs)/rN)
		featureIndexer.addStable("report-doc-quarter", (4*ps)/pN)
		featureIndexer.addStable("same-doc-quarter", bool2value((4*rs)/rN == (4*ps)/pN))

		val isHead = (m: Mention) => bool2value(m.getHeadTokenIdx == m.getStartTokenIdx)
		featureIndexer.addStable("report-isHead", isHead(reportMention))
		featureIndexer.addStable("passage-isHead", isHead(passageMention))
		featureIndexer.addStable("both-isHead", isHead(reportMention) * isHead(passageMention))

		val rn = reportMention.getEndTokenIdx - reportMention.getStartTokenIdx
		val pn = passageMention.getEndTokenIdx - passageMention.getStartTokenIdx
		(0 until 5).foreach(k => {
			featureIndexer.addStable("report-numWords"+k.toString, bool2value(rn == k+1))
			featureIndexer.addStable("passage-numWords"+k.toString, bool2value(pn == k+1))
		})
		featureIndexer.addStable("same-numWords", bool2value(rn == pn))

		val rsp = reportMention.getHeadTokenIdx / 4
		val psp = passageMention.getHeadTokenIdx / 4
		featureIndexer.addStable("report-sentence-pos", rsp)
		featureIndexer.addStable("passage-sentence-pos", psp)
		featureIndexer.addStable("both-sentence-pos", (rsp*4)+psp)

		featureIndexer.commit
	}
}

