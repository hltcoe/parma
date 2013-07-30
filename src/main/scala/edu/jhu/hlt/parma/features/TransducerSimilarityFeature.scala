// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.parma.inference.transducers.StringEditModel
import edu.jhu.hlt.parma.inference.transducers.AnnotatedString
import java.util.logging.Logger
import java.io.{ File, FileInputStream }
import java.io.ObjectInputStream
import scala.collection.mutable
import scala.collection.JavaConversions._

class TransducerSimilarityFeature extends AlignmentSimilarity {
  
	val MODEL_PATH = ParmaConfig.getFile("features.transducer.model")
	val alphabet = new Alphabet
	var transducer: Option[StringEditModel] = None
	def isSetup = !transducer.isEmpty

	override def setup(calibrateOn: java.util.Collection[DocAlignment]) {
		log.info("loading transducer models...")
		transducer = Some(readModel(MODEL_PATH))
		log.info("done loading transducer models")
	}

	def normalize(logp: Double): Double = {
		// If there are two long dissimilar strings, the probability will
		// underflow.
		if(logp == Double.NegativeInfinity) {
			log.warning("underflow in transducer prob calc")
			-100.0
		}
		else if(logp == Double.PositiveInfinity) {
			log.warning("positive infinity in transducer prob calc")
			-100.0
		}
		else if(logp == Double.NaN) {
			log.warning("NaN in transducer prob calc")
			-300.0
		}
		else logp / 50d		// crude attempt to put on scale with other features
	}

	def score(x_str: String, y_str: String): Double = {
		if(!isSetup) {
			log.warning("ad-hoc set up! no examples")
			setup(Seq())
		}
		val x = new AnnotatedString(x_str)
		val y = new AnnotatedString(y_str)
		try {
			var logp = transducer.get.logp(x,y)
			var logpb = transducer.get.logp(y,x)
			//log.info("x_str="+x_str+" y_str="+y_str+" logp=" + logp + " logpb=" + logpb)

			// nick says that max tends to work better than sum
			//return normalize(logp) + normalize(logpb)
			return math.max(normalize(logp), normalize(logpb))

		} catch {
			case e: ArrayIndexOutOfBoundsException =>
			//e.printStackTrace
			//log.warning("[Transducer score] ArrayIndexOutOfBoundsException x=%s, y=%s".format(x_str, y_str))
			return -100d
		}
	}

	private[this] val binarizer = new FixedWidthBinarizer(15, false, -40d, -1d)
	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {

		assert(isSetup)

		val (reportCM, passageCM) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val rh = report.getHeadString(reportCM)
		val ph = passage.getHeadString(passageCM)

		val (f1, f2) =
			if(a.isInstanceOf[ArgCorefAlignment]) {
				val aca = a.asInstanceOf[ArgCorefAlignment]
				val ms = aca.reportCoref.flatMap(ra =>
					aca.passageCoref.map(pa =>
					score(report.getHeadString(ra), passage.getHeadString(pa))))
				(ms.min, ms.sum / ms.size.toDouble)
			}
			else (binarizer.agnostic, binarizer.agnostic)
		featureIndexer.start(sv)
		featureIndexer.addStable("transducer-logp-head-canonical", score(rh, ph), binarizer)
		featureIndexer.addStable("transducer-logp-head-coref-min", f1, binarizer)
		featureIndexer.addStable("transducer-logp-head-coref-avg", f2, binarizer)
		featureIndexer.commit
	}

	def readModel(file: File) : StringEditModel = {
		println("[TransducerSimilarityFeature] loading model from " + file.getPath)
		val input = new ObjectInputStream(new FileInputStream(file))
		val model = input.readObject().asInstanceOf[StringEditModel]
        AnnotatedString.setAlphabet(model.getAlphabet())
		input.close
		model
	}
}

