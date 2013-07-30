// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.experiments

import edu.jhu.hlt.parma.annotation.MTurkUtils
import edu.jhu.hlt.parma.math.{Stats, ConfidenceInterval}
import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.diagnostics._
import edu.jhu.hlt.parma.evaluation._
import collection.mutable.HashMap
import collection.mutable.ArrayBuffer
import java.io._

object PipelineRunner {
	// for some reason this doesn't appear to work because of
	// how i'm calling java/scala from a shell script
	// (leaving this out because I already call Profiler.writeoutTimes below)
	/*
	Runtime.getRuntime.addShutdownHook(new Thread {
		override def run {
			println("inside shutdown hook")
			Profiler.writeoutTimes
		}
	})
	*/

    util.Random.setSeed(RandomUtils.randomSeed)
    // can't do this for java Random...

	def main(args: Array[String]) {
		val pipeline = new Pipeline[FeatureRepresentation]
		pipeline.main(args)
	}
}

/**
 * runs experiments specified in parma.config
 */
class Pipeline[F <: FeatureRepresentation] extends Logging {
	
	val modelSerializeKey = "diagnostics.serialize.model"
	
	def getExperiments: Seq[Experiment[InferenceEngine[F]]] = {
		ParmaConfig.getStrings(ParmaConfig.EXPERIMENTS).map(expName => {
			val fullName =
				if(expName.startsWith("edu.jhu.hlt.parma")) expName
				else "edu.jhu.hlt.parma.experiments." + expName
			try {
				Class.forName(fullName).newInstance.asInstanceOf[Experiment[InferenceEngine[F]]]
			} catch {
				case cnf: ClassNotFoundException =>
					throw new RuntimeException("could not find experiment named: " + fullName)
			}
		})
	}

	def main(args: Array[String]) {
		var success = false
		if(args.length == 1) {
			log("loading config file: " + args(0))
			ParmaConfig.load(args(0))
		}
		Profiler.startTask("total")
		GeneralDiagnostics.checkConfig
		try {
			for(experiment <- getExperiments)
				run(experiment)
			success = true
		} catch {
			case e: Exception => {
				log("grep for this for exceptions")
				e.printStackTrace
			}
		}
		Profiler.endTask("total")
		Profiler.writeoutTimes
		System.exit(if(success) 0 else -1)
	}
	
	def run(experiment: Experiment[InferenceEngine[F]]): InferenceEngine[F] = {

		val engine = experiment.inferenceEngine
		//val engine = loadInferenceEngine
		
		var data: DocAlignmentCorpus[_ <: DocAlignment] = Profiler.time("loadData", Unit => experiment.rawData)
		GeneralDiagnostics.checkCorpus(data)
		GeneralDiagnostics.printCorpusStatistics(data)
		
		// calibrate features / whatever else
		Profiler.startTask("preTrainCalibrate")
		engine.preTrainCalibrate(data.trainAlignments ++ data.devAlignments)
		Profiler.endTask("preTrainCalibrate")
		
		// TODO check if train and test alignments all have the same domain
		// if so, then we want to make sure that we don't use domain adaptation
		// so that we don't double our feature vector size and slow things down
		val domains = data.allAlignments.groupBy(_.domain)
		for((dom, as) <- domains)
			println("[Pipeline] domain %s has %d alignments ".format(dom, as.size))
		if(domains.size > 1)
			throw new RuntimeException("implement me")
		
		// compute features and keep them for the rest of the experiment
		Profiler.startTask("computeFeatures")
		log("about to compute features on all the data (this may take a while)...")
		val featureDumpDir = ParmaConfig.getDirectory("diagnostics.features.serialize", null)
		var pCount = 0
		def promote(da: DocAlignment) = {
			//Profiler.startTask("computeFeaturesForOneAlignment")
			val start = System.currentTimeMillis
			val dawf = engine.computeFeatures(da)
			//val t = Profiler.endTask("computeFeaturesForOneAlignment") / 1000d
			val t = (System.currentTimeMillis - start)/1000d
			println("[computeFeatures in Pipeline] " + Describe.memoryUsage(timestamp=true) + ", " +
				pCount + " / " + data.totalSize + ", took " + t + " seconds")
			pCount += 1
			// TODO retur to this, need to serialize a whole crapload of stuff in a dawf
			// maybe force feature representations or inference engines to implement (de)serialize
			//if(featureDumpDir != null) {
			//	val f = new File(featureDumpDir, da.id + ".fv.gz")
			//	FeatureVectorIO.toFile(fv, f)
			//}
			dawf
		}
		val featurizedData = data.map(promote, "_wFeatures", verbose=true)
		val featureComputeTime = Profiler.endTask("computeFeatures") / 1000d
		log("done! took %.1f seconds".format(featureComputeTime))
		//data = null; System.gc
		

		// slice and dice the data however you want (given that the features are already computed)
		val losses = new ArrayBuffer[Double]
		val results = new HashMap[String, Seq[Double]]
		CosineVsF1.open
		implicit def fda2da(fda: DocAlignmentWithFeatures[F]): DocAlignment = fda.alignment
		for((split, splitNum) <- experiment.evaluationSplits(featurizedData).zipWithIndex) {
			println("#train = " + split.train.size)
			println("#dev = " + split.dev.size)
			println("#test = " + split.test.size)
			val (l, r) = runOnCorpus(engine, experiment, split)
			losses += l
			for((k,v) <- r)
				results(k) = results.getOrElse(k, Seq()) :+ v
		}
		CosineVsF1.close


		println("average loss on %s = %s".format(experiment.name, new ConfidenceInterval(losses).toString))
		for((k, vs) <- results)
			println("average on %s, %s = %s".format(experiment.name, k, new ConfidenceInterval(vs).toString))

		saveInferenceEngine(engine)
		engine
	}


	def saveInferenceEngine(engine: InferenceEngine[F]) {
		val f = ParmaConfig.getFile(modelSerializeKey)
		if(f != null && f.exists) {
			if(engine.isInstanceOf[Serializable]) {
				log("saving model to " + f.getPath)
				val oos = new ObjectOutputStream(new FileOutputStream(f))
				oos.writeObject(engine)
				oos.close
			}
			else warning("you asked to save %s to %s, but %s is not serializable!"
				.format(engine.name, f.getPath, engine.getClass.getName))
		}
		else log("not saving model because %s was not set in parma.config".format(modelSerializeKey))
	}

	def loadInferenceEngine: InferenceEngine[_] = {
		val f = ParmaConfig.getFile(modelSerializeKey)
		if(f != null && f.exists) {
			log("loading model from " + f.getPath)
			val ois = new ObjectInputStream(new FileInputStream(f))
			val model = ois.readObject.asInstanceOf[InferenceEngine[_]]
			ois.close
			model
		}
		else throw new RuntimeException("cannot find file: " + modelSerializeKey)
	}

	
	/**
	 * returns loss given by experiment.loss
	 */
	def runOnCorpus(engine: InferenceEngine[F],
			experiment: Experiment[InferenceEngine[F]],
			corpus: Corpus[DocAlignmentWithFeatures[F]]): (Double, HashMap[String, Double]) = {
		
		// train (and maybe tune on dev)
		log("runOnCorpus about to train on %d examples...".format(corpus.train.size))
		Profiler.time("train", Unit => engine.train(corpus.train))
		if(corpus.dev.size > 0) {
			log("runOnCorpus about to devTune on %d examples...".format(corpus.dev.size))
			Profiler.time("devTune", Unit => engine.postTrainCalibrate(corpus.dev, experiment.loss _))
		}

		// produce alignments on test data and evaluate
		Profiler.startTask("evaluation")
		if(corpus.test.size == 0) {
			warning("no test examples were give, nothing to evaluate on")
			(0d, new HashMap[String, Double])
		}
		else {
			log("runOnCorpus about to test")
			
			val predictions = corpus.test.map(dawf => engine.align(dawf.features))
			val instances = corpus.test.map(_.alignment).zip(predictions).map(gold_hyp =>
				new Instance(gold_hyp._2, gold_hyp._1))
			
			for((dawf, inst) <- corpus.test.zip(instances))
				FeatureDiagnostics.writeoutAlignmentFeatures(inst, dawf.features)
			MTurkUtils.dumpAlignments(predictions, corpus.id)
			CosineVsF1.analyze(instances, corpus.id)
			CosineBySentenceVsF1.analyze(instances, corpus.id)
			
			val results = new HashMap[String, Double]	// run evaluation functions specified by experiment
			for(func <- experiment.evaluationFunctions) {
				val (name, score) = func(instances)
				results.put(name, score)
				log("experiment=%s, corpus=%s, %s=%.3f".format(experiment.name, corpus.id, name, score))
			}
			
			val loss = experiment.loss(instances)
			log("experiment=%s, corpus=%s, loss=%.3f".format(experiment.name, corpus.id, loss))
			GeneralDiagnostics.outputPredictions(instances)
			engine.writeoutParameters(experiment.name + "-on-"+corpus.id)
			Profiler.endTask("evaluation")
			(loss, results)
		}
	}
	
}


