// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

import edu.jhu.hlt.parma.util.VecOps

class DVec(private[this] var items: Array[Double]) extends Iterable[(Int, Double)] with Serializable {
	def this(vs: Double*) = this(vs.toArray[Double])
	val dimension: Int = items.length

	/**
	 * WARNING: best not to use this if possible
	 */
	def getArray: Array[Double] = items

	def l2: Double = math.sqrt(items.map(x => x*x).sum)
	def l1: Double = items.map(math.abs).sum
	def apply(i: Int): Double = items(i)
	def ++(other: DVec) = VecOps.concat(this, other)
	override def iterator: Iterator[(Int, Double)] = (0 until dimension).iterator.map(i => (i, items(i)))
	override def toString: String = "dv[" + items.mkString(", ") + "]"
	def linearTransform(scale: Double, shift: Double): DVec =
		new DVec(items.map(x => x*scale + shift))
	def scale(factor: Double) = linearTransform(factor, 0d)
	def *=(factor: Double) {
		var i = 0
		while(i < dimension) {
			items(i) *= factor
			i += 1
		}
	}
	def +=(other: DVec) {
		assert(dimension == other.dimension)
		var i = 0
		while(i < dimension) {
			items(i) += other(i)
			i += 1
		}
	}
	def update(idx: Int, value: Double) { items(idx) = value }
	def setBacking(buf: Array[Double]) { items = buf }
	def minEquals(other: DVec) {
		assert(dimension == other.dimension)
		var i = 0
		while(i < dimension) {
			val z = other(i)
			if(z < items(i))
				items(i) = z
			i += 1
		}
	}
	def maxEquals(other: DVec) {
		assert(dimension == other.dimension)
		var i = 0
		while(i < dimension) {
			val z = other(i)
			if(z > items(i))
				items(i) = z
			i += 1
		}
	}
	def copy: DVec = {
		val na = Array.ofDim[Double](items.length)
		Array.copy(items, 0, na, 0, items.length)
		new DVec(na)
	}
	def copyTo(buf: Array[Double]) {
		assert(buf.length == dimension)
		java.lang.System.arraycopy(items, 0, buf, 0, items.length)
	}
}

object DVec {
	def zero(i: Int): DVec = new DVec(Array.ofDim[Double](i))
	val empty = zero(0)
	val zero1 = zero(1)
	val zero2 = zero(2)
	val zero3 = zero(3)
	val zero4 = zero(4)
	val zero5 = zero(5)
	def rep(value: Double, n: Int) = new DVec(Array.fill(n)(value))

	def max(a: DVec, b: DVec): DVec = {
		if(a.dimension != b.dimension)
			throw new RuntimeException("must match in dimension: %d != %d".format(a.dimension, b.dimension))
		val n = a.dimension
		val c = Array.ofDim[Double](n)
		var i = 0
		while(i < n) {
			c(i) = if(a(i) > b(i)) a(i) else b(i)
			i += 1
		}
		new DVec(c)
	}

	def sum(a: DVec, b: DVec): DVec = {
		if(a.dimension != b.dimension)
			throw new RuntimeException("must match in dimension: %d != %d".format(a.dimension, b.dimension))
		val n = a.dimension
		val c = Array.ofDim[Double](n)
		var i = 0
		while(i < n) {
			c(i) = a(i) + b(i)
			i += 1
		}
		new DVec(c)
	}
}

