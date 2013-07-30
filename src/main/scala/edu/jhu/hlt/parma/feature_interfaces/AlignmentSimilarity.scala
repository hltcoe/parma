// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.feature_interfaces

import edu.jhu.hlt.parma.types._
import java.util.logging._

/**
 * this trait provides features that describe how similar
 * two mentions are (on their own -- there will be factors
 * that get to look at more than a pair of mentions later)
 * 
 * this trait is replacing AlignmentSimiliarityFunction
 * 
 * all new feature functions should extend this trait if
 * possible. if not talk to Travis about a java wrapper
 */
trait AlignmentSimilarity extends Serializable {

	@transient
	val log = Logger.getLogger(this.getClass.getName)

	// use this in featurize()
	// you may override this value if you want keys other than strings
	protected val featureIndexer = new FeatureIndexer[String]

	/**
	 * give the name of the feature for this index
	 * (this index corresponds to the indices in SVec
	 *  modified by featurize())
	 *
	 * this is the crux of the feature-index-name contract
	 * you dump values into an SVec in featurize(),
	 * and you don't need to worry about collisions,
	 * but you need to be able to provide the name of a feature
	 * given its index.
	 *
	 * featureIndexer has been included to help you do this
	 * (see various feature implementations or the FeatureIndexer
	 * source for information on how to use it),
	 * but in special cases you may want to implement your own
	 * method and ignore featureIndexer
	 */
	def featureName(index: Int): String =
		featureIndexer.lookupObject(index)

	/**
	 * call this function for the value of a binary feature function
	 * this value may change, e.g. to [-1, 1] or const * [0, 1], in the
	 * future, so please use this to avoid hard coding a value everywhere
	 */
  	def bool2value(b: Boolean): Double = {
		if(b) 1d
		else 0d
	}
  
	/**
	 * please don't let this collide with any other features,
	 * or Bad Things will happen
	 */
	def name: String = this.getClass.getName.replace("edu.jhu.hlt.parma.features.", "")

	def setup(calibrateOn: java.util.Collection[DocAlignment]) {}
	
	def cleanup {}

	/**
	 * see FeatureIndexer for details on how to add to SVec,
	 * (use this.featureIndexer to do so)
	 */
	def featurize(sv: SVec, a: Alignment, report: Document, passage: Document)

	def statelessFeaturize(a: Alignment, report: Document, passage: Document): SVec = {
		val sv = new SVec
		featurize(sv, a, report, passage)
		sv
	}
}


