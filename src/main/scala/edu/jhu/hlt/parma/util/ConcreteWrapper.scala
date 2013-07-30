// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.concrete.Concrete._
import edu.jhu.hlt.concrete.io.{ ProtocolBufferReader, ProtocolBufferWriter }
import java.io._
import java.util.zip._
import collection.mutable.ArrayBuffer

object ConcreteWrapper {

	def writeCommunicationsTo(f: File, comms: Seq[Communication]) {
		println("writing %d Communications to %s".format(comms.size, f.getPath))
		val pbw = new ProtocolBufferWriter(f.getPath)
		comms.foreach(pbw.write)
		pbw.close
	}
	
	def getCommunicationsFrom(f: File): IndexedSeq[Communication] =
		getCommunicationsFrom(f, (c: Communication) => true)

	/**
	 * only keeps in memory Communications for which keep returns true
	 * can be used with very large protobuf files
	 */
	def getCommunicationsFrom(f: File, keep: Communication => Boolean): IndexedSeq[Communication] = {
		val pbr = getCommReader(f)
		val buf = new ArrayBuffer[Communication]
		while(pbr.hasNext) {
			val comm = pbr.next
			if(keep(comm))
				buf += comm
		}
		pbr.close
		buf.toIndexedSeq
	}

	def getDocumentsFrom(f: File): IndexedSeq[Document] =
		getDocumentsFrom(f, (d: Document) => true)
	
	/**
	 * only keeps in memory Documents for which keep returns true
	 * can be used with very large protobuf files
	 */
	def getDocumentsFrom(f: File, keep: Document => Boolean): IndexedSeq[Document] = {
		val pbr = getCommReader(f)
		val buf = new ArrayBuffer[Document]
		while(pbr.hasNext) {
			val doc = new RichConcreteDoc(pbr.next)
			if(keep(doc))
				buf += doc
		}
		pbr.close
		buf.toIndexedSeq
	}

	private def getCommReader(f: File) = {
		if(!f.exists || !f.isFile)
			throw new RuntimeException("please provide a valid file: " + f.getPath)
		val fis = new FileInputStream(f)
		val is =
			if(f.getName.toLowerCase.endsWith("gz"))
				new GZIPInputStream(fis)
			else fis
		println("[ConcreteWrapper getCommReader] reading from " + f.getPath)
		println(Describe.memoryUsage(true))
		new ProtocolBufferReader[Communication](is, classOf[Communication])
	}

}

