// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.experiments

import scala.collection.mutable.ArrayBuffer

object EvaluationSplits {
	
	type SplitFunc[T] = Corpus[T] => Seq[Corpus[T]]
	
	/**
	 * same rules as leave one out
	 */
	def crossValidation[T](k: Int): SplitFunc[T] = {
		(c: Corpus[T]) => {
			val corps = new ArrayBuffer[Corpus[T]]
			for(i <- 0 until k) {
				val te = c.test.zipWithIndex.filter(_._2 % k == i).map(_._1)
				val tr = c.test.zipWithIndex.filter(_._2 % k != i).map(_._1)
				corps += new Corpus(c.id+"-fold"+i, c.train ++ tr, c.dev, te)
			}
			corps
		}
	}
	
	/**
	 * dev will be kept in dev
	 * CV will occur on test items, anything in train
	 * will stay in train
	 */
	def leaveOneOut[T](k: Int): SplitFunc[T] = {
		(c: Corpus[T]) => {
			for(t <- c.test)
				yield new Corpus(c.id, c.train ++ c.test.filter(_ != t), c.dev, Seq(t))
		}
	}
	
	def asIs[T] = (tdt: Corpus[T]) => Seq(tdt)

}

