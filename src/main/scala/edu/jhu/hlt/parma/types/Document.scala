// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.concrete.Concrete
import edu.jhu.hlt.concrete.Concrete._
import edu.jhu.hlt.concrete.util.IdUtil
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.collection.JavaConversions._

trait CommunicationDocument extends Document {

	def communication: Concrete.Communication
	
	/** keys are Concrete.Entity ids */
	def entityMapping: Bijection[UUID, ArgumentCoref]

	/** keys are Concrete.Situation ids */
	def situationMapping: Bijection[UUID, Predicate]
}

trait DocumentBuilder extends Document {

	// useful when changing things about a document
	def deepCopy(): DocumentBuilder = deepCopy(id)
	def deepCopy(newId: String): DocumentBuilder

	// useful for DocAlignmentPerturber
	// if you use this, you are not gauranteed anything with
	// respect to consistency of the document (e.g. subbing out
	// a sentence that has a predicate changes what that predicate
	// refers to)
	def setSentence(idx: Int, sent: Sentence)

	def addPredicate(p: Predicate)
	def addArgument(a: Argument)
	def addCoref(ac: ArgumentCoref)
}

class RichConcreteDoc(val id: String, override val communication: Concrete.Communication) extends CommunicationDocument {

	def this(c: Concrete.Communication) = this(c.getGuid.getCommunicationId, c)

	private[this] val entMap = new MutableBijection[UUID, ArgumentCoref]
	private[this] val sitMap = new MutableBijection[UUID, Predicate]
	private[this] val sentBuf = new ArrayBuffer[Sentence]

	{
		// add sentences
		val t2s = new MutableBijection[UUID, Sentence]
		val sectSegmentations = communication.getSectionSegmentationList
		assert(sectSegmentations.size == 1)
		for(section <- sectSegmentations.head.getSectionList) {
			val sentSegmentations = section.getSentenceSegmentationList
			assert(sentSegmentations.size == 1)
			for(cSent <- sentSegmentations.head.getSentenceList) {
				val s = new RichConcreteSent(cSent, sentBuf.size)
				sentBuf += s
				val toks = cSent.getTokenizationList
				assert(toks.size == 1)
				t2s.add(toks.head.getUuid, s)
			}
		}

		// pull out relevant annotations to work with
		val annos = ConcreteUtils.getParmaAnnotations(communication)

		// populate mentions (for later lookup)
		val mentions = new MutableBijection[UUID, Mention]	// keys are EntityMention and SituationMention ids
		for(em <- annos.ems.getMentionList) {
			val trs = em.getTokens
			val sentence = t2s.getForwards(trs.getTokenizationId)
			val m = MentionBuilder.from(sentence, trs)
			mentions.add(em.getUuid, m)
		}
		for(sm <- annos.sms.getMentionList) {
			val trs = sm.getTokens
			val sentence = t2s.getForwards(trs.getTokenizationId)
			val m = MentionBuilder.from(sentence, trs)
			mentions.add(sm.getUuid, m)
		}

		// add arguments
		for(ent <- annos.es.getEntityList) {
			val coref = new ArrayBuffer[Argument]
			for(mentUUID <- ent.getMentionIdList) {
				val mention = mentions.getForwards(mentUUID)
				coref += new Argument(mention)
			}
			entMap.add(ent.getUuid, new ArgumentCoref(coref))
		}

		// add predicates
		for(sit <- annos.ss.getSituationList) {
			for(sitMentUUID <- sit.getMentionIdList) {
				val mention = mentions.getForwards(sitMentUUID)
				sitMap.add(sit.getUuid, new Predicate(mention))
			}
		}

	}

	/** keys are Concrete.Entity ids */
	override def entityMapping: Bijection[UUID, ArgumentCoref] = entMap.freezeSafe

	/** keys are Concrete.Situation ids */
	override def situationMapping: Bijection[UUID, Predicate] = sitMap.freezeSafe

	override val sentences = sentBuf.toIndexedSeq
	override val corefs = entMap.rightValues.toIndexedSeq
	override val arguments = corefs.flatMap(_.chain)
	override val predicates = sitMap.rightValues.toIndexedSeq
}

/**
 * mutable, builder for Document
 * does not modify commToAnnotate
 */
class RichConcreteDocBuilder(override val id: String, private val commToAnnotate: Concrete.Communication)
		extends DocumentBuilder
		with CommunicationDocument
		with Logging {

	def this(cd: Concrete.Communication) =
		this(cd.getGuid.getCommunicationId, cd)

	override val sentences: IndexedSeq[Sentence] = {
		val s = new ArrayBuffer[Sentence]
		var sentIdx = 0
		val sectSegmentations = commToAnnotate.getSectionSegmentationList
		assert(sectSegmentations.size == 1)
		for(section <- sectSegmentations.head.getSectionList) {
			val sentSegmentations = section.getSentenceSegmentationList
			assert(sentSegmentations.size == 1)
			for(cSent <- sentSegmentations.head.getSentenceList) {
				s += new RichConcreteSent(cSent, sentIdx)
				sentIdx += 1
			}
		}
		s.toIndexedSeq
	}

	override def deepCopy(newId: String): RichConcreteDocBuilder =
		new RichConcreteDocBuilder(newId, commToAnnotate.toBuilder.build)

	override def setSentence(idx: Int, s: Sentence) {
		// TODO i'm going to have not store a Communication, but
		// rather the parts of it that are relevant, e.g. ArrayBuffer[Sentence]
		// TODO check for preds/args in this sentence
		throw new RuntimeException("implement me")
	}

	// state/cache
	private[this] var commWithPAs: Communication = null
	private[this] var entMentionSet: EntityMentionSet = null
	private[this] var sitMentionSet: SituationMentionSet = null
	private[this] val entMap = new MutableBijection[UUID, ArgumentCoref]
	private[this] val sitMap = new MutableBijection[UUID, Predicate]

	val debugPreds = new java.util.HashSet[Predicate]

	private[this] val addedPreds = new ArrayBuffer[PredicateSituation]
	override def predicates = addedPreds.map(_.pred).toIndexedSeq
	override def addPredicate(p: Predicate) {
		val cs = getSentence(p).asInstanceOf[RichConcreteSent]
		addedPreds += new PredicateSituation(p, cs)
		assert(debugPreds.add(p))
		dirty
	}

	private[this] val addedArgs = new ArrayBuffer[ArgumentEntity]
	override def arguments = addedArgs.map(_.arg).toIndexedSeq
	/**
	 * @deprecated
	 * this method has the semantics that you are adding a coref
	 * chain of size 1. You almost certainly should not be using
	 * this, but rather building up HalfAlignments and using
	 * AnnotationAligner.makeDocAlignment
	 */
	override def addArgument(a: Argument) {
		warning("addArgument() is deprecated!")
		val cs = getSentence(a).asInstanceOf[RichConcreteSent]
		//addedArgs += new ArgumentEntity(a, cs)
		addedCorefs += new ArgCorefEntity(Seq(new ArgumentEntity(a, cs)))
		dirty
	}
	
	private[this] val addedCorefs = new ArrayBuffer[ArgCorefEntity]
	override def corefs = addedCorefs.map(_.coref).toIndexedSeq
	override def addCoref(c: ArgumentCoref) {
		addedCorefs += new ArgCorefEntity(c.chain.map(arg => {
			val cs = getSentence(arg).asInstanceOf[RichConcreteSent]
			new ArgumentEntity(arg, cs)
		}))
		dirty
	}

	class PredicateSituation(val pred: Predicate, val sent: RichConcreteSent) {
		val mention: SituationMention = Concrete.SituationMention.newBuilder
			.setUuid(IdUtil.generateUUID)
			.setTokens(ConcreteParmaConverters.toTokenRefSeq(pred.location, sent))
			.build
		val situation: Situation = Concrete.Situation.newBuilder
			.setUuid(IdUtil.generateUUID)
			.addMentionId(mention.getUuid)
			.build
		def longString(doc: Document): String = "(PredicateSituation pred=%s sit=%s text=%s)".format(pred, situation, Describe.mentionInContext(pred.location, doc))
	}

	class ArgumentEntity(val arg: Argument, val sent: RichConcreteSent) {
		val mention: EntityMention = Concrete.EntityMention.newBuilder
			.setUuid(IdUtil.generateUUID)
			.setTokens(ConcreteParmaConverters.toTokenRefSeq(arg.location, sent))
			.build
		// can build an entity here because these are considered singletons
		val entity: Entity = Concrete.Entity.newBuilder
			.setUuid(IdUtil.generateUUID)
			.addMentionId(mention.getUuid)
			.build
	}

	class ArgCorefEntity(val chain: Seq[ArgumentEntity]) {
		private val eb = Concrete.Entity.newBuilder
			.setUuid(IdUtil.generateUUID)
		chain.foreach(ae => eb.addMentionId(ae.mention.getUuid))
		val entity: Entity = eb.build
		val coref: ArgumentCoref = new ArgumentCoref(chain.map(_.arg))
		val mentions: Seq[EntityMention] = chain.map(_.mention)
	}

	/**
	 * dump cached stateful fields
	 */
	private def dirty {
		commWithPAs = null
		entMentionSet = null
		sitMentionSet = null
		entMap.clear
		sitMap.clear
	}

	/**
	 * update the stateful fields
	 */
	private def clean {

		if(commWithPAs != null)
			return

		// copy the original communication, keeping the ID(s)
		val cb = commToAnnotate.toBuilder

		// ====== SITUATIONS ======
		// add SituationMentionSet
		val sms = Concrete.SituationMentionSet.newBuilder
			.setUuid(IdUtil.generateUUID)
			.setMetadata(ConcreteParmaConverters.defaultMetadata)
		addedPreds.foreach(p => sms.addMention(p.mention))
		cb.addSituationMentionSet(sms)

		// add SituationSet
		val ss = Concrete.SituationSet.newBuilder
			.setUuid(IdUtil.generateUUID)
			.setMetadata(ConcreteParmaConverters.defaultMetadata)
		addedPreds.foreach(p => ss.addSituation(p.situation))
		cb.addSituationSet(ss)

		// update Situation.uuid <=> Predicate mapping
		addedPreds.foreach(p => sitMap.add(p.situation.getUuid, p.pred))

		// ====== ENTITIES =======
		// add EntityMentionSet
		val ems = Concrete.EntityMentionSet.newBuilder
			.setUuid(IdUtil.generateUUID)
			.setMetadata(ConcreteParmaConverters.defaultMetadata)
		addedArgs.foreach(x => ems.addMention(x.mention))
		addedCorefs.flatMap(_.mentions).foreach(ems.addMention)
		cb.addEntityMentionSet(ems)

		// add EntitySet
		val es = Concrete.EntitySet.newBuilder
			.setUuid(IdUtil.generateUUID)
			.setMetadata(ConcreteParmaConverters.defaultMetadata)
		addedArgs.foreach(ae => es.addEntity(ae.entity))
		addedCorefs.foreach(c => es.addEntity(c.entity))
		cb.addEntitySet(es)

		// update Entity.uuid <=> Argument(Coref) mapping
		addedCorefs.foreach(ac => entMap.add(ac.entity.getUuid, ac.coref))

		commWithPAs = cb.build
	}

	override def communication: Concrete.Communication = {
		clean
		commWithPAs
	}
	
	/** keys are Concrete.Entity ids */
	override def entityMapping: Bijection[UUID, ArgumentCoref] = {
		clean
		entMap.freezeSafe
	}

	/** keys are Concrete.Situation ids */
	override def situationMapping: Bijection[UUID, Predicate] = {
		clean
		sitMap.freezeSafe
	}
}

/**
 * immutable
 *
 * NOTE: most code should use this type, not a more specific type
 * only use subclasses if you need to change the internals of this type
 * if that is the case, you should have access to a subclass of this which is a builder
 */
trait Document {

	def id: String
	def sentences: IndexedSeq[Sentence]
	def predicates: IndexedSeq[Predicate]
	def arguments: IndexedSeq[Argument]
	def corefs: IndexedSeq[ArgumentCoref]

	def getSentence(idx: Int): Sentence = sentences(idx)
	def getSentence(predicate: Predicate): Sentence = getSentence(predicate.location)
	def getSentence(argument: Argument): Sentence = getSentence(argument.location)
	def getSentence(mention: Mention): Sentence = sentences(mention.getSentenceIdx)

	def getMentionTokens(predicate: Predicate): IndexedSeq[Token] = getMentionTokens(predicate.location)
	def getMentionTokens(argument: Argument): IndexedSeq[Token] = getMentionTokens(argument.location)
	def getMentionTokens(mention: Mention): IndexedSeq[Token] = {
		assert(mention.getStartTokenIdx < mention.getEndTokenIdx)
		getSentence(mention).tokens.slice(mention.getStartTokenIdx, mention.getEndTokenIdx)
	}

	def getMentionString(predicate: Predicate): String = getMentionString(predicate.location)
	def getMentionString(argument: Argument): String = getMentionString(argument.location)
	def getMentionString(mention: Mention): String = {
		getMentionTokens(mention).map(_.getWord).mkString(" ")
	}

	def getHeadToken(predicate: Predicate): Token = getHeadToken(predicate.location)
	def getHeadToken(argument: Argument): Token = getHeadToken(argument.location)
	def getHeadToken(mention: Mention): Token = {
		val t = getSentence(mention).tokens
		t(mention.getHeadTokenIdx)
	}

	def getHeadString(predicate: Predicate): String = getHeadString(predicate.location)
	def getHeadString(argument: Argument): String = getHeadString(argument.location)
	def getHeadString(mention: Mention): String = getHeadToken(mention).getWord
	
	def governs(m: Mention) = getSentence(m).governs(m)
	
	def governedBy(m: Mention) = getSentence(m).governedBy(m)
	
	def mentionHeadToken(m: Mention): Token =
		getSentence(m).tokens.get(m.getHeadTokenIdx)

	def rawString: String = sentences.map(_.rawString).mkString("\n")
	def allTokens: Seq[Token] = sentences.flatMap(_.tokens)

	override val hashCode: Int = id.hashCode
	override def equals(other: Any): Boolean = {
		if(other.isInstanceOf[Document]) {
			val o = other.asInstanceOf[Document]
			id == o.id //&&
				//sentences == o.sentences &&
				//predicates == o.predicates &&
				//arguments == o.arguments &&
				//corefs == o.corefs
		}
		else false
	}
	
}


