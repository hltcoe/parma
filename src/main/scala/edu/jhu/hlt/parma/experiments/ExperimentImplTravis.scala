// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.experiments

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util.{Describe, ParmaConfig}
import edu.jhu.hlt.parma.input._
import edu.jhu.hlt.parma.evaluation._
import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.features._
import edu.jhu.hlt.parma.diagnostics.BadAlignmentDiagnostics
import util.Random
import collection.mutable.ArrayBuffer

class RFLemmaDevSet extends Experiment[NoTrainAligner] {
	lazy val devAlignments = new RothFrankDocReader(true, false) getAlignedDocuments
	override def rawData = new DocAlignmentCorpus("RF_dev", Seq(), Seq(), devAlignments.toSeq)
	override def inferenceEngine = new NoTrainAligner(new LemmaMatch)
}

class RFLemmaTestSet extends Experiment[NoTrainAligner] {
	lazy val testAlignments = new RothFrankDocReader(false, true) getAlignedDocuments
	override def rawData = new DocAlignmentCorpus("RF_test", Seq(), Seq(), testAlignments.toSeq)
	override def inferenceEngine = new NoTrainAligner(new LemmaMatch)
}

class RFOnlyExperiment extends Experiment[HierarchicalAlignmentModule] {
	lazy val trainAlignments = new RothFrankDocReader(true, false) getAlignedDocuments
	lazy val testAlignments = new RothFrankDocReader(false, true) getAlignedDocuments
	override val rawData = new DocAlignmentCorpus("trainRF_testRF", trainAlignments.toSeq, Seq(), testAlignments.toSeq)
	override def inferenceEngine = new HierarchicalAlignmentModule
}


class EECBCVExperiment extends Experiment[HierarchicalAlignmentModule] {
	val reader = new ConcreteDocReader(ParmaConfig.getFile("data.eecb.concrete.alignments"))
	val (dev, test) = DocAlignmentCorpus.randomSplit(reader.getAlignedDocuments.toSeq, propDev)
	override def rawData = new DocAlignmentCorpus("eecb", Seq(), dev, test)
  	override def inferenceEngine = new HierarchicalAlignmentModule
	override def evaluationSplits[T](c: Corpus[T])(implicit asDA: T => DocAlignment) =
		EvaluationSplits.crossValidation(cvFolds)(c)
}

class EECBLemmaExperiment extends Experiment[NoTrainAligner] {
	override def rawData = new DocAlignmentCorpus("eecb", Seq(), Seq(), EECBDocReader.getAlignedDocuments.toSeq)
  	override def inferenceEngine = new NoTrainAligner(new LemmaMatch)
}

class EECBTrainFrankTestExperiment extends Experiment[HierarchicalAlignmentModule] {
	lazy val trainAlignments = EECBDocReader.getAlignedDocuments
	lazy val testAlignments = new RothFrankDocReader(false, true) getAlignedDocuments
	override val rawData = new DocAlignmentCorpus("trainEECB_testRFdev", trainAlignments.toSeq, Seq(), testAlignments.toSeq)
	override def inferenceEngine = new HierarchicalAlignmentModule
}

class MixtureTrainRFTestExperimentWithLDC extends MixtureTrainRFTestExperiment {
	override def train: Seq[DocAlignment] =
		super.train ++ LeastOverlapData.alignments.take(300)
}


/**
 * use this experiment just to train a model, not evalutate
 */
class TurkerEvalTrain extends Experiment[HierarchicalAlignmentModule] {
	override def inferenceEngine = new HierarchicalAlignmentModule
	//private val dev = Seq[DocAlignment]()	//new RothFrankDocReader(true, false).getAlignedDocuments.toSeq
	//private val train = EECBDocReader.getAlignedDocuments.take(3)
	private val dev = new RothFrankDocReader(true, false).getAlignedDocuments.toSeq
	private val train = EECBDocReader.getAlignedDocuments ++ (new RothFrankDocReader(false, true).getAlignedDocuments)
	override def rawData = new DocAlignmentCorpus("turker-eval-train", train, dev, Seq())
}


class MixtureTrainRFTestExperiment extends Experiment[HierarchicalAlignmentModule] {

	override def inferenceEngine = new HierarchicalAlignmentModule

	val test = new RothFrankDocReader(false, true).getAlignedDocuments.toSeq
	val rfDev = new RothFrankDocReader(true, false).getAlignedDocuments.toSeq
	val dev = rfDev ++
			Random.shuffle(EECBDocReader.getAlignedDocuments.toSeq).take(10) ++
			Random.shuffle(LeastOverlapData.alignments).take(10)
	def train: Seq[DocAlignment] = Random.shuffle(EECBDocReader.getAlignedDocuments.toSeq).take(300)
	override def rawData = new DocAlignmentCorpus("trainMix_testRF", train, dev, test)

	override def evaluationSplits[T](c: Corpus[T])(implicit asDocAlignment: T => DocAlignment): Seq[Corpus[T]] = {
		val rfDomain = new RothFrankDocReader(true, false).domain
		val (devOOD, devID) = c.dev.partition(_.domain != Some(rfDomain))
		assert(devOOD.size > 0)	// "out of domain"
		assert(devID.size > 0)	// "in domain"
		Seq(1, 5, 15).flatMap(devReps => {
			val dv = devOOD ++ (1 to devReps).flatMap(i => devID)
			Seq(5, 15, 50).map(trainReps => {
				val tr = c.train ++ (1 to trainReps).flatMap(i => devID)
				val id = "%s-dev%d-train%d".format(c.id, devReps, trainReps)
				new Corpus(id, tr, dv, c.test)
			})
		})
	}
}

class LeastOverlapLemma extends Experiment[NoTrainAligner] {
	override def rawData = new DocAlignmentCorpus("LeastOverlapLemma", Seq(), Seq(), LeastOverlapData.alignments)
  	override def inferenceEngine = new NoTrainAligner(new LemmaMatch)
}

class LeastOverlapCV extends Experiment[HierarchicalAlignmentModule] {
	override def rawData = {
		val (dev, test) = DocAlignmentCorpus.randomSplit(LeastOverlapData.alignments, propDev)
		new DocAlignmentCorpus("LeastOverlapCV", Seq(), dev, test)
	}
	override def inferenceEngine = new HierarchicalAlignmentModule
	override def evaluationSplits[T](c: Corpus[T])(implicit asDA: T => DocAlignment) =
		EvaluationSplits.crossValidation(cvFolds)(c)
}

object LeastOverlapData {
	def alignments: Seq[ParametricDocAlignment[DocumentBuilder]] = {

		val outOfDomainDocs = EECBDocReader.getAlignedDocuments.flatMap(da => List(da.report, da.passage)).toSet.toIndexedSeq

		var all = LeastOverlapReader.getAlignedDocuments
		all = DocAlignmentPerturber.leastOverlapSubset(all, 50)
		println("[LeastOverlapTesting] all alignments = " + all.size)

		//val leastOverlap = DocAlignmentPerturber.leastOverlapSubset(all, 1000)
		val thresh = 0.5d
		val leastOverlap = DocAlignmentPerturber.lowOverlap(all, thresh)
		println("[LeastOverlapTesting] after choosing doc alignments with overlappiness<%.1f = %d".format(thresh, leastOverlap.size))

		var alignments = leastOverlap.map(da => DocAlignmentPerturber.degradeDocAlignment(da, outOfDomainDocs))
		println("[LeastOverlapTesting] done degrading alignments")

		alignments = alignments.filter(da => {
			da.report.predicates.size > 0 && da.report.arguments.size > 0 &&
			da.passage.predicates.size > 0 && da.passage.arguments.size > 0
		})
		println("[LeastOverlapTesting] after filtering docs no preds/args = " + alignments.size)

		val tooManyPredArgs = 80
		alignments = alignments.filter(da => {
			da.report.predicates.size < tooManyPredArgs && da.passage.predicates.size < tooManyPredArgs &&
			da.report.arguments.size < tooManyPredArgs && da.passage.arguments.size < tooManyPredArgs
		})
		println("[LeastOverlapTesting] after filtering docs with too many preds/args = " + alignments.size)

		//val total = 300
		//alignments = Random.shuffle(alignments).take(total)
		//println("[LeastOverlapTesting] done, taking " + total)

		for(da <- alignments) {
			println("[Experiments LeastOverlapTesting] report.#preds=%d report.#args=%d passage.#preds=%d passage.#args=%d"
			   .format(da.report.predicates.size, da.report.arguments.size, da.passage.predicates.size, da.passage.arguments.size))
		}

		println("[Experiments LeastOverlapTesting] done getting data!")

		alignments
	}
}

class PolyTrain extends Experiment[HierarchicalAlignmentModule] {
	override def rawData = {
		val buf = new ArrayBuffer[DocAlignment]
		for(ds <- Seq("global-voices", "roth_frank", "eecb")) {
			val f = ParmaConfig.getFile("data.%s.concrete.alignments".format(ds))
			buf ++= new ConcreteDocReader(f, Some(ds)).getAlignedDocuments
		}
		val (test, trainDev) = DocAlignmentCorpus.randomSplit(buf.toSeq, propTest)
		val (dev, train) = DocAlignmentCorpus.randomSplit(trainDev, propDev)
		new DocAlignmentCorpus("PolyTrain", train, dev, test)
	}
	override def inferenceEngine = new HierarchicalAlignmentModule
}

class GVCV extends Experiment[HierarchicalAlignmentModule] {
	override def rawData = {
		val f = ParmaConfig.getFile("data.global-voices.concrete.alignments")
		val all = new ConcreteDocReader(f, Some("GV")).getAlignedDocuments
		//val (dev, train) = DocAlignmentCorpus.randomSplit(all, propDev)
		//new DocAlignmentCorpus("GVCV", Seq(), dev, train)

		//val (test, trainDev) = DocAlignmentCorpus.randomSplit(all, propTest)
		//val (dev, train) = DocAlignmentCorpus.randomSplit(trainDev, propTest)
		//new DocAlignmentCorpus("GVCV", train, dev, test)

		val (train, test) = DocAlignmentCorpus.randomSplit(all, 0.6d)
		new DocAlignmentCorpus("GVCV", train, Seq(), test)
	}
	override def inferenceEngine = new HierarchicalAlignmentModule
	//override def evaluationSplits[T](c: Corpus[T])(implicit asDA: T => DocAlignment) =
	//	EvaluationSplits.crossValidation(cvFolds)(c)
}

