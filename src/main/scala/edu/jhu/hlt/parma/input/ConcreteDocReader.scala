// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import java.io.File

class ConcreteDocReader(var file: File, domain: Option[String] = None) extends DocumentReader[DocAlignment] {
	def this() = this(ParmaConfig.getFile("data.concrete.input"))
	override def getAlignedDocuments: Seq[DocAlignment] = {
		println("[ConcreteDocReader] reading from " + file.getPath)
		val das = ConcreteUtils.deserialize(file)
		domain match {
			case None => das
			case Some(str) =>
				// replace the original domain, which will always be "Annotated Gigaword"
				das.map(da => new DocAlignment(da.id, Some(str), da.report, da.passage, da.sureAlignments, da.exactlyPossibleAlignments))
		}
	}
}

