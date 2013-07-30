// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

// Nicholas Andrews, Johns Hopkins University
// noa@jhu.edu

package edu.jhu.hlt.parma.inference.transducers;

import edu.jhu.hlt.parma.inference.transducers.AnnotatedString;
import edu.jhu.hlt.parma.util.Alphabet;

public interface StringEditModel {
    
    public void setAlphabet(Alphabet<Character> A);
    public Alphabet<Character> getAlphabet();

    public double logp(AnnotatedString input, AnnotatedString output);
    public double [] logp(AnnotatedString [] inputs, AnnotatedString [] outputs);
    public double calc_ll(AnnotatedString [] inputs, AnnotatedString [] outputs, double [] weights);
    public double calc_ll(AnnotatedString [] inputs, AnnotatedString [] outputs);

    public double em_step(AnnotatedString [] inputs, AnnotatedString [] outputs, double [] weights);

    public void train(AnnotatedString [] train_inputs, AnnotatedString [] train_outputs,
                      AnnotatedString [] test_inputs,  AnnotatedString [] test_outputs);
    public AnnotatedString sample(AnnotatedString input);
}