// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

// Nicholas Andrews, Johns Hopkins University
// noa@jhu.edu

package edu.jhu.hlt.parma.inference.transducers;

import edu.jhu.hlt.parma.inference.transducers.MersenneTwisterFast;

public class RandomNumberGenerator {
    static MersenneTwisterFast rng = new MersenneTwisterFast(0);
    static public void setSeed(int seed) {
        rng.setSeed(seed);
    }
    static public double nextDouble() {
        return rng.nextDouble();
    }
    static public float nextFloat() {
        return rng.nextFloat();
    }
    static public int nextInt(int total) {
        return rng.nextInt(total);
    }
    static public double nextGaussian() {
        return rng.nextGaussian();
    }
    static public MersenneTwisterFast getGenerator() {
        return rng;
    }
}