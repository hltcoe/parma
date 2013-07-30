// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.{ CanonicalMentionFinder, DocMetaAligner }
import edu.jhu.hlt.parma.util.ConcreteParmaConverters._
import edu.jhu.hlt.concrete.Concrete._
import edu.jhu.hlt.concrete.util.IdUtil
import collection.mutable.ArrayBuffer
import collection.JavaConversions._
import java.util.Calendar
import java.io._

object ConcreteUtils {

	/**
	 * parma only wants to operate on one hypothesis from Concrete
	 * this class specifies one hypothesis about a Communication.
	 */
	class SingleCommunicationAnnotation(
		val ems: EntityMentionSet,
		val es: EntitySet,
		val sms: SituationMentionSet,
		val ss: SituationSet)

	/**
	 * strict makes sure that there is only one parma annotation,
	 * and throws an exception if there isn't. if not strict, it
	 * chooses the last parma annotation.
	 */
	def getParmaAnnotations(c: Communication, strict: Boolean = true): SingleCommunicationAnnotation = {

		// parmas annotations
		val emsl = c.getEntityMentionSetList.filter(x => ConcreteParmaConverters.isParmaMetadata(x.getMetadata))
		val esl = c.getEntitySetList.filter(x => ConcreteParmaConverters.isParmaMetadata(x.getMetadata))
		val smsl = c.getSituationMentionSetList.filter(x => ConcreteParmaConverters.isParmaMetadata(x.getMetadata))
		val ssl = c.getSituationSetList.filter(x => ConcreteParmaConverters.isParmaMetadata(x.getMetadata))

		if(strict && (emsl.size > 1 || esl.size > 1 || smsl.size > 1 || ssl.size > 1))
			throw new RuntimeException

		// default to any other annotations if parmas are not available
		val ems =
			if(emsl.size > 0) {
				if(strict && emsl.size > 1) throw new RuntimeException
				emsl.last
			}
			else c.getEntityMentionSetList.last

		val es =
			if(esl.size > 0) {
				if(strict && esl.size > 1) throw new RuntimeException
				esl.last
			}
			else c.getEntitySetList.last

		val sms =
			if(smsl.size > 0) {
				if(strict && smsl.size > 1) throw new RuntimeException
				smsl.last
			}
			else c.getSituationMentionSetList.last

		val ss =
			if(ssl.size > 0) {
				if(strict && ssl.size > 1) throw new RuntimeException
				ssl.last
			}
			else c.getSituationSetList.last

		new SingleCommunicationAnnotation(ems, es, sms, ss)
	}

	/**
	 * read a bunch of doc alignments from a file.
	 * file must have been written by serialize()
	 * NOTE: parma-proprietary, does not play nice with Concrete utils
	 */
	def deserialize(f: File): Seq[DocAlignment] = {
		val alignments = new ArrayBuffer[DocAlignment]
		val dis = new DataInputStream(new FileInputStream(f))
		val n = dis.readInt	// how many DocAlignments in this file
		log("[ConcreteUtils deserialize] about to read %d DocAlignments as Discourses and Communications from %s"
			.format(n, f.getPath))
		if(n <= 0) throw new RuntimeException("n = " + n)
		for(i <- 1 to n) {
			val discourse = Discourse.parseDelimitedFrom(dis)
			val report = Communication.parseDelimitedFrom(dis)
			val passage = Communication.parseDelimitedFrom(dis)
			if(i % 100 == 1) {
				log("[ConcreteUtils.deserialize] %d/%d report=%s passage=%s"
					.format(i, n, report.getGuid.getCommunicationId, passage.getGuid.getCommunicationId))
				log(Describe.memoryUsage())
			}
			alignments += ConcreteParmaConverters.fromDiscourse(discourse, report, passage)
		}
		dis.close
		alignments.toSeq
	}

	/**
	 * save a bunch of doc alignments to a file
	 * NOTE: parma-proprietary, does not play nice with Concrete utils
	 */
	def serialize(das: Seq[DocAlignment], f: File) {
		log("[ConcreteUtils serialize] about to write %d DocAlignments as Discourses and Communications to %s"
			.format(das.size, f.getPath))
		val dos = new DataOutputStream(new FileOutputStream(f))
		dos.writeInt(das.size)	// how many DocAlignments in this file
		for(da <- das) {
			val report = da.report.asInstanceOf[CommunicationDocument]
			val passage = da.passage.asInstanceOf[CommunicationDocument]
			val discourse = ConcreteParmaConverters.toDiscourse(da, report, passage)
			discourse.writeDelimitedTo(dos)
			report.communication.writeDelimitedTo(dos)
			passage.communication.writeDelimitedTo(dos)
		}
		dos.close
	}
}

