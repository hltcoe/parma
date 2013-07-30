
PARMA: A Predicate-Argument Aligner
===================================

Copyright (c) 2013, Johns Hopkins University. All rights reserved.
This software is released under the 2-clause BSD license.

Parma is a tool for aligning mentions of predicates (i.e. events)
and arguments (i.e. entities) in two given documents.
It is explained in more detail in
[this paper](http://www.cs.jhu.edu/~vandurme/papers/PARMA:ACL:2013.pdf).
This package is primarily for research purposes, and its primary
goal extensibility and flexibility rather than usability by non-researchers.

To reference this package in academic writing:
```
@inproceedings{wolfe2013parma,
	author = {Travis Wolfe and Benjamin Van Durme and Mark Dredze and Nicholas Andrews and Charley Beller and Chris Callison-Burch and Jay DeYoung and Justin Snyder and Jonathan Weese and Tan Xu and Xuchen Yao},
	title = {PARMA: A Predicate Argument Aligner},
	booktitle = {ACL Short Papers 2013},
	year = {2013}
}
```

To use parma, the easiest way is probably to do a local maven
install, as we do not have it deployed on a public maven server yet.
Parma also builds with sbt, and `sbt console` is very useful
(see `util.Describe` for detailed toString methods).

for a top-down view into how the code works, see (in order)
* CLI.scala
* experiments/Experiment.scala
* experiments/Pipeline.scala
* types/*.scala
* inference/InferenceEngine.scala
* inference/HierarchicalAlignmentModule.scala
* util/ParmaConfig.scala
* anything else you want to look at

If you find a bug or wish to contribute, feel free to contact
the me via email (twolfe18@gmail.com) or Github (twolfe).

