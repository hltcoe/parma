// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.types._

object VecOps {

	/**
	 * dv += scale * sv
	 */
	def add(dv: DVec, sv: SVec, scale: Double) {
		sv.items.foreach(iv => dv(iv._1) += scale * iv._2)
	}

	def dot(dv: DVec, sv: SVec): Double = {
		var dot = 0d
		sv.items.foreach(iv => dot += dv(iv._1) * iv._2)
		dot
	}

	def concat(a: DVec, b: DVec): DVec = {
		val an = a.dimension
		val bn = b.dimension
		val c = Array.ofDim[Double](an + bn)
		var i = 0
		while(i < an) {
			c(i) = a(i)
			i += 1
		}
		var j = 0
		while(j < bn) {
			c(i) = b(j)
			i += 1
			j += 1
		}
		new DVec(c)
	}

	/**
	 * buf += this * scale
	 * use this in tight gradient loops, etc
	 */
	def addWithScale(src: SVec, dest: Array[Double], scale: Double) {
		val indices = src.getIndices
		val values = src.getValues
		var i = 0
		val n = indices.length
		while(i < n) {
			dest(indices(i)) += values(i) * scale
			i += 1
		}
	}
}

