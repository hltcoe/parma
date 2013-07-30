// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

trait Logging {
	var v = 1
	def log(msg: String, verbosity: Int = 1, sender: String = null) {
		if(verbosity >= v) {
			if(sender != null)
				println("[%s] %s".format(sender, msg))
			else println(msg)
		}
	}
	def warning(msg: String, sender: String = null) {
		if(sender != null)
			println("[%s] %s".format(sender, msg))
		else println("[WARNING] " + msg)
	}
}

