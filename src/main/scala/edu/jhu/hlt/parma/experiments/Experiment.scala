// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.experiments

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.input._
import edu.jhu.hlt.parma.evaluation._
import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.features._
import edu.jhu.hlt.parma.diagnostics.BadAlignmentDiagnostics
import scala.util.Random

/**
 * basically a factory for a bunch of stuff that Pipeline uses
 */
trait Experiment[T <: InferenceEngine[_]] extends Logging {
  
	val verbose = false

	def name: String = this.getClass.getName.replace("edu.jhu.hlt.parma.experiments.", "")
	
	/**
	 * gets the data for the experiment
	 */
	def rawData: DocAlignmentCorpus[_ <: DocAlignment]
 
	def cvFolds = {
		val k = ParmaConfig.getInt("experiments.cvFolds", 5)
		assert(k > 1)
		k
	}
  	def propTrain = ParmaConfig.getDouble("experiments.propTrain", 0.75d)
  	def propDev = ParmaConfig.getDouble("experiments.propDev", 0.15d)
  	def propTest = ParmaConfig.getDouble("experiments.propTest", 0.25d)
	
	/**
	 * how should I produce alignments?
	 */
	def inferenceEngine: T
	
 	/**
	 * what should we optimize?
	 * defaults to F1
	 */
  	def loss(examples: Seq[Instance[DocAlignment]]): Double = {
	
		val f = (i: Instance[DocAlignment]) => SetBasedEvaluator.generousF1(i, takePreds=true, takeArgCorefs=true)
		val unif = 1d - SetBasedEvaluator.macroAvg(examples, f)
		val weighted = 1d - SetBasedEvaluator.microAvg(examples, f, (da: DocAlignment) => da.sureAlignments.size)
		
		val percentDiff = (unif - weighted)*2 / (unif + weighted)
		if(verbose && math.abs(percentDiff) > 0.2)
			warning("percent diff = %.2f, unif = %.2f, weighted = %.2f".format(percentDiff, unif, weighted))
		//assert(math.abs(percentDiff) < 0.2d)

		weighted
	}

	
	type EvaluationFunction = Seq[Instance[DocAlignment]] => (String, Double)
	
	/**
	 * return a list of evaluation metrics to run
	 * return type should be a name and a number
	 * 
	 * defaults are pretty exhaustive, but you can override
	 * this method by adding functions to it
	 */
	def evaluationFunctions: List[EvaluationFunction] = {
		List(
	
			// Micro
			// F1
			(examples: Seq[Instance[DocAlignment]]) =>
				("micro-F1", SetBasedEvaluator.microAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousF1(i, takePreds = true, takeArgCorefs = true),
					(da: DocAlignment) => da.sureAlignments.size.toDouble)),
			(examples: Seq[Instance[DocAlignment]]) =>
				("micro-pred-F1", SetBasedEvaluator.microAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousF1(i, takePreds = true, takeArgCorefs = false),
					(da: DocAlignment) => da.sureAlignments.size.toDouble)),
			(examples: Seq[Instance[DocAlignment]]) =>
				("micro-arg-F1", SetBasedEvaluator.microAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousF1(i, takePreds = false, takeArgCorefs = true),
					(da: DocAlignment) => da.sureAlignments.size.toDouble)),

			// Precision
			(examples: Seq[Instance[DocAlignment]]) =>
				("micro-precision", SetBasedEvaluator.microAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousPrecision(i, takePreds = true, takeArgCorefs = true),
					(da: DocAlignment) => da.sureAlignments.size.toDouble)),
			(examples: Seq[Instance[DocAlignment]]) =>
				("micro-pred-precision", SetBasedEvaluator.microAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousPrecision(i, takePreds = true, takeArgCorefs = false),
					(da: DocAlignment) => da.sureAlignments.size.toDouble)),
			(examples: Seq[Instance[DocAlignment]]) =>
				("micro-arg-precision", SetBasedEvaluator.microAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousPrecision(i, takePreds = false, takeArgCorefs = true),
					(da: DocAlignment) => da.sureAlignments.size.toDouble)),

			// Recall
			(examples: Seq[Instance[DocAlignment]]) =>
				("micro-recall", SetBasedEvaluator.microAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousRecall(i, takePreds = true, takeArgCorefs = true),
					(da: DocAlignment) => da.sureAlignments.size.toDouble)),
			(examples: Seq[Instance[DocAlignment]]) =>
				("micro-pred-recall", SetBasedEvaluator.microAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousRecall(i, takePreds = true, takeArgCorefs = false),
					(da: DocAlignment) => da.sureAlignments.size.toDouble)),
			(examples: Seq[Instance[DocAlignment]]) =>
				("micro-arg-recall", SetBasedEvaluator.microAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousRecall(i, takePreds = false, takeArgCorefs = true),
					(da: DocAlignment) => da.sureAlignments.size.toDouble)),



			// Macro
			// F1
			(examples: Seq[Instance[DocAlignment]]) =>
				("macro-F1", SetBasedEvaluator.macroAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousF1(i, takePreds = true, takeArgCorefs = true))),
			(examples: Seq[Instance[DocAlignment]]) =>
				("macro-pred-F1", SetBasedEvaluator.macroAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousF1(i, takePreds = true, takeArgCorefs = false))),
			(examples: Seq[Instance[DocAlignment]]) =>
				("macro-arg-F1", SetBasedEvaluator.macroAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousF1(i, takePreds = false, takeArgCorefs = true))),

			// Precision
			(examples: Seq[Instance[DocAlignment]]) =>
				("macro-precision", SetBasedEvaluator.macroAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousPrecision(i, takePreds = true, takeArgCorefs = true))),
			(examples: Seq[Instance[DocAlignment]]) =>
				("macro-pred-precision", SetBasedEvaluator.macroAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousPrecision(i, takePreds = true, takeArgCorefs = false))),
			(examples: Seq[Instance[DocAlignment]]) =>
				("macro-arg-precision", SetBasedEvaluator.macroAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousPrecision(i, takePreds = false, takeArgCorefs = true))),

			// Recall
			(examples: Seq[Instance[DocAlignment]]) =>
				("macro-recall", SetBasedEvaluator.macroAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousRecall(i, takePreds = true, takeArgCorefs = true))),
			(examples: Seq[Instance[DocAlignment]]) =>
				("macro-pred-recall", SetBasedEvaluator.macroAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousRecall(i, takePreds = true, takeArgCorefs = false))),
			(examples: Seq[Instance[DocAlignment]]) =>
				("macro-arg-recall", SetBasedEvaluator.macroAvg(examples, (i: Instance[DocAlignment]) =>
					SetBasedEvaluator.generousRecall(i, takePreds = false, takeArgCorefs = true))),

			// Other
			(examples: Seq[Instance[DocAlignment]]) =>
				("true-positives", (examples.map(SetBasedEvaluator.truePos).sum.toDouble)),
			(examples: Seq[Instance[DocAlignment]]) =>
				("true-negatives", (examples.map(SetBasedEvaluator.trueNeg).sum.toDouble)),
			(examples: Seq[Instance[DocAlignment]]) =>
				("false-positives", (examples.map(SetBasedEvaluator.falsePos).sum.toDouble)),
			(examples: Seq[Instance[DocAlignment]]) =>
				("false-negatives", (examples.map(SetBasedEvaluator.falseNeg).sum.toDouble)),
			(examples: Seq[Instance[DocAlignment]]) =>
				("bad-doc-alignments", (BadAlignmentDiagnostics.printOutDiagnostics(examples).toDouble))
		)
	}
	
	/**
	 * provide the train-dev-test splits that you want the pipeline to execute
	 * 
	 * assume that there is a 1-1 between Ts and DAs
	 * define a partition based on DAs, map it back to Ts
	 * (T will typically be a feature representation)
	 */
	def evaluationSplits[T](c: Corpus[T])(implicit asDocAlignment: T => DocAlignment): Seq[Corpus[T]] =
		EvaluationSplits.asIs(c)
	
}

