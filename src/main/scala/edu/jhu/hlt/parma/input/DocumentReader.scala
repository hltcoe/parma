// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.types._
import java.io.File

trait DocumentReader[DA <: DocAlignment] {
	
	/**
	 * returns gold alignments
	 */
	def getAlignedDocuments: Seq[DA]
	
	/**
	 * give the domain string, used for domain adaptation
	 */
	def domain: String = this.getClass.getName.replaceAll("edu.jhu.hlt.parma.input.", "")
}

trait ClusteredDocReader[DA <: DocAlignment] extends DocumentReader[DA] {
	type ClusterIdType = String
	
	/**
	 * returns gold alignments with clusters that should be
	 * respected for train/dev/test/CV splits
	 */
	def getClusteredAlignedDocuments: scala.collection.Map[ClusterIdType, Seq[DA]]
	
	override def getAlignedDocuments = getClusteredAlignedDocuments.values.toSeq.flatten
}

