// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.concrete.Concrete._
import edu.jhu.hlt.concrete.util._
import collection.mutable.ArrayBuffer
import collection.JavaConversions._
import java.io.DataOutputStream
import java.util.Calendar

object ConcreteParmaConverters extends Logging {
	
	var verbose = false

	val annotationTime = Calendar.getInstance.getTimeInMillis / 1000
	val meta = AnnotationMetadata.newBuilder
		.setTimestamp(annotationTime)
		.setTool("parma")
		.setConfidence(1f) 
		.build
	
	def defaultMetadata = meta

	def isParmaMetadata(am: AnnotationMetadata): Boolean = {
		val t = am.getTool
		t != null && t == "parma"
	}

	def toTokenRefSeq(mention: Mention, sent: RichConcreteSent): TokenRefSequence = {
		assert(sent.concreteSent.getTokenizationList.size == 1)
		val tid = sent.concreteSent.getTokenizationList.get(0).getUuid
		val b = TokenRefSequence.newBuilder
			.setTokenizationId(tid)
			.setAnchorTokenIndex(mention.getHeadTokenIdx)
		for(i <- mention.getStartTokenIdx until mention.getEndTokenIdx)
			b.addTokenIndex(i)
		b.build
	}

	// DocAlignment => Discourse
	def toDiscourse(da: DocAlignment, report: CommunicationDocument, passage: CommunicationDocument): Discourse = {
		val dab = Discourse.DiscourseAnnotation.newBuilder
			.setId(IdUtil.generateUUID)
			.setMetadata(meta)
	
		if(verbose) log("writing discourse for " + da.id)

		// arguments/entities
		val reportEntMap: Bijection[UUID, ArgumentCoref] = report.entityMapping
		val passageEntMap: Bijection[UUID, ArgumentCoref] = passage.entityMapping
		for(aca <- da.argCorefAlignments) {
			
			val reportArgCoref = aca.reportCoref
			val passageArgCoref = aca.passageCoref

			val reportEntRef = EntityRef.newBuilder
				.setEntityId(reportEntMap.getBackwards(reportArgCoref))
				.setCommunicationId(report.communication.getUuid)

			val passageEntRef = EntityRef.newBuilder
				.setEntityId(passageEntMap.getBackwards(passageArgCoref))
				.setCommunicationId(passage.communication.getUuid)

			val conf = if(da.sureAlignments.contains(aca)) 1f else 0.5f

			dab.addDiscourseEntity(DiscourseEntity.newBuilder
				.setId(IdUtil.generateUUID)
				.setConfidence(conf)
				.addEntityRef(reportEntRef)
				.addEntityRef(passageEntRef))
		}

		// predicates/situations
		val reportSitMap: Bijection[UUID, Predicate] = report.situationMapping
		val passageSitMap: Bijection[UUID, Predicate] = passage.situationMapping
		for(pa <- da.predicateAlignments) {

			val reportPred = pa.reportPred
			val passagePred = pa.passagePred

			val reportSitRef = SituationRef.newBuilder
				.setSituationId(reportSitMap.getBackwards(reportPred))
				.setCommunicationId(report.communication.getUuid)

			val passageSitRef = SituationRef.newBuilder
				.setSituationId(passageSitMap.getBackwards(passagePred))
				.setCommunicationId(passage.communication.getUuid)

			val conf = if(da.sureAlignments.contains(pa)) 1f else 0.5f

			dab.addDiscourseSituation(DiscourseSituation.newBuilder
				.setId(IdUtil.generateUUID)
				.setConfidence(conf)
				.addSituationRef(reportSitRef)
				.addSituationRef(passageSitRef))
		}

		Discourse.newBuilder
			.setId(IdUtil.generateUUID)
			.setMetadata(meta)
			.addAnnotation(dab)
			.build
	}

	// Discourse => DocAlignment
	def fromDiscourse(discourse: Discourse, reportComm: Communication, passageComm: Communication): DocAlignment = {

		val report = new RichConcreteDoc(reportComm)
		val passage = new RichConcreteDoc(passageComm)

		val sureAlignments = new ArrayBuffer[Alignment]
		val possibleAlignments = new ArrayBuffer[Alignment]
		def addAlignment(a: Alignment, confidence: Double) {
			assert(0 <= confidence && confidence <= 1)
			if(confidence >= 0.75)
				sureAlignments += a
			else if(confidence >= 0.5)
				possibleAlignments += a
		}

		assert(discourse.getAnnotationList.size == 1)
		val da = discourse.getAnnotationList.head

		// argument coref alignments
		val reportEntMap: Bijection[UUID, ArgumentCoref] = report.entityMapping
		val passageEntMap: Bijection[UUID, ArgumentCoref] = passage.entityMapping
		for(discEnt <- da.getDiscourseEntityList) {
			assert(discEnt.getEntityRefList.size == 2)
			var reportArgCoref: ArgumentCoref = null
			var passageArgCoref: ArgumentCoref = null
			for(entRef <- discEnt.getEntityRefList) {
				val entUUID = entRef.getEntityId
				val commUUID = entRef.getCommunicationId
				if(commUUID == report.communication.getUuid)
					reportArgCoref = reportEntMap.getForwards(entUUID)
				else {
					assert(commUUID == passage.communication.getUuid)
					passageArgCoref = passageEntMap.getForwards(entUUID)
				}
			}
			val aca = new ArgCorefAlignment(reportArgCoref, passageArgCoref)
			addAlignment(aca, discEnt.getConfidence)
		}

		// predicate alignments
		val reportSitMap: Bijection[UUID, Predicate] = report.situationMapping
		val passageSitMap: Bijection[UUID, Predicate] = passage.situationMapping
		for(discSit <- da.getDiscourseSituationList) {
			assert(discSit.getSituationRefList.size == 2)
			var reportPred: Predicate = null
			var passagePred: Predicate = null
			for(sitRef <- discSit.getSituationRefList) {
				val sitUUID = sitRef.getSituationId
				val commUUID = sitRef.getCommunicationId
				if(commUUID == report.communication.getUuid)
					reportPred = reportSitMap.getForwards(sitUUID)
				else {
					assert(commUUID == passage.communication.getUuid)
					passagePred = passageSitMap.getForwards(sitUUID)
				}
			}
			val pa = new PredicateAlignment(reportPred, passagePred)
			addAlignment(pa, discSit.getConfidence)
		}

		val id = "r%s_p%s".format(report.id, passage.id)
		val domain = Some(report.communication.getGuid.getCorpusName)
		assert(report.communication.getGuid.getCorpusName == passage.communication.getGuid.getCorpusName)
		new DocAlignment(id, domain, report, passage, sureAlignments.toSet, possibleAlignments.toSet)
	}

}

