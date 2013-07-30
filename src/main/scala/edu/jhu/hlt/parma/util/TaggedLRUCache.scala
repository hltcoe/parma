// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import collection.mutable.ArrayBuffer

/**
 * LRU cache with "approximate set behavior" dictated by
 * a tag provided upon adding items. Elements should be
 * immutable (tag is never updated).
 * Set behavior is that upon adding an element with tag=X,
 * if there are any other elements in the cache with tag=X,
 * the item will not be added (and nothing will be evicted).
 */
class TaggedLRUCache[T](val maxSize: Int) extends Iterable[T] {

	private[this] val cache = new ArrayBuffer[T]
	private[this] val tags = Array.ofDim[Long](maxSize)
	private[this] var idx = 0

	def containsTag(tag: Long): Boolean = {
		val n = size
		var i = 0
		while(i < n) {
			if(tag == this.tags(i))
				return true
			i += 1
		}
		return false
	}

	/**
	 * returns true if the set did not already contain this item (tag)
	 */
	def add(item: T, tag: Long): Boolean = {
		if(!containsTag(tag)) {
			if(cache.size < maxSize)
				cache += item
			else {
				cache(idx) = item
				idx = if(idx+1 == maxSize) 0 else idx+1
			}
			true
		}
		else false
	}

	override def iterator: Iterator[T] = cache.iterator

	override def size = cache.size

	def clear {
		cache.clear
		idx = 0
	}

}

