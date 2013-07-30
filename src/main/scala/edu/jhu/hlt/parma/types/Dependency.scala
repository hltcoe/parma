// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

class Dependency[T](val typ: String, val gov: T, val dep: T) {
	def toUntyped = new Dependency[T]("untyped", gov, dep)
	def map[B](f: T => B): Dependency[B] = new Dependency(typ, f(gov), f(dep))
	override def toString: String = "(Dep t=%s g=%s d=%s)".format(typ, gov, dep)
	override def equals(obj: Any): Boolean = {
		if(obj.isInstanceOf[Dependency[_]]) {
			val o = obj.asInstanceOf[Dependency[_]]
			o.typ == typ && o.gov == gov && o.dep == dep
		}
		else false
	}
	override def hashCode: Int = (typ.hashCode << 20) | (gov.hashCode << 10) | (dep.hashCode)
}

