// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference.transducers;

import edu.jhu.hlt.parma.util.*;
import java.util.*;

import org.apache.commons.math3.stat.regression.SimpleRegression;
import org.apache.commons.math3.stat.Frequency;

//import flanagan.analysis.Regression;

/**
 * TODO Currently this does not operate in log-space.
 * 
 * @author Spence Green
 *
 * @param <K>
 */
public class UnigramLM<K> {

	private final Counter<K> lm;
	private double UNK_PROB = 0.0;
	private final Set<K> unkTokens;
	private static final double kCutoff = 5.0;
	private static final double unkCutoff = 1.0;

	public UnigramLM() {
	    lm = new Counter<K>();
	    unkTokens = new HashSet<K>();
	}

	public void normalize() {
	    //Counters.normalize(lm);
		// MY COUNTER IS ALWAYS NORMALIZED AND AWESOME
	}
	
	public int vocabSize() {
	    return lm.keySet().size();
	}
	
	public boolean contains(K token) { return lm.containsKey(token); }
	
	public Set<K> getVocab() { return Collections.unmodifiableSet(lm.keySet()); }

	public double getCount(K token) {
	    if(!lm.keySet().contains(token)) {
			System.err.println(lm.keySet().size());
			throw new RuntimeException("token not in keyset");
	    }
	    return lm.getCount(token);
	}
	
	/**
	 * GT smoothing with least squares interpolation. This follows the
	 * procedure in Jurafsky and Martin sect. 4.5.3.
	 */
	public void smoothAndNormalize() {
		Counter<Integer> cntCounter = new Counter<Integer>();
		for(K tok : lm.keySet()) {
			int cnt = (int) lm.getCount(tok);
			cntCounter.incrementCount(cnt);
		}

		final double[] coeffs = runLogSpaceRegression(cntCounter);

		UNK_PROB = cntCounter.getCount(1) / lm.totalCount();

		for(K tok : lm.keySet()) {
			double tokCnt = lm.getCount(tok);
			if(tokCnt <= unkCutoff) //Treat as unknown
				unkTokens.add(tok);
			if(tokCnt <= kCutoff) { //Smooth
				double cSmooth = katzEstimate(cntCounter, tokCnt, coeffs);
				lm.setCount(tok, cSmooth);
			}
		}

		//Normalize
		//Counters.normalize(lm);
		// MY COUNTER IS ALWAYS NORMALIZED AND AWESOME
	}

	private double katzEstimate(Counter<Integer> cnt, double c, double[] coeffs) {
		double nC = cnt.getCount((int) c);
		double nC1 = cnt.getCount(((int) c) + 1);
		if(nC1 == 0.0)
			nC1 = Math.exp(coeffs[0] + (coeffs[1] * (c+1.0)));
			
		double n1 = cnt.getCount(1);
		double nK1 = cnt.getCount(((int) kCutoff) + 1);
		if(nK1 == 0.0)
			nK1 = Math.exp(coeffs[0] + (coeffs[1] * (kCutoff+1.0)));

		double kTerm = (kCutoff + 1.0) * (nK1 / n1);
		double cTerm = (c + 1.0) * (nC1 / nC);

		double cSmooth = (cTerm - (c * kTerm)) / (1.0 - kTerm);

		return cSmooth;
	}

	/**
	 * Does the regression in log-space.
	 * 
	 * @param cntCounter
	 * @return
	 */
	// private double[] runLogSpaceRegression(Counter<Integer> cntCounter) {
	// 	double[] xData = new double[cntCounter.keySet().size()];
	// 	double[] yData = new double[cntCounter.keySet().size()];
	// 	double[] wts = new double[cntCounter.keySet().size()];
	// 	int dataPtr = 0;
	// 	for(int cnt : cntCounter.keySet()) {
	// 		xData[dataPtr] = cnt;
	// 		yData[dataPtr++] = Math.log(cntCounter.getCount(cnt));
	// 	}
	// 	Regression reg = new Regression(xData,yData,wts);
	// 	reg.linear();
	// 	double[] coeffs = reg.getBestEstimates();
	// 	assert coeffs.length == 2;

	// 	return coeffs;
	// }

    private double [] runLogSpaceRegression(Counter<Integer> cntCounter) {
        SimpleRegression reg = new SimpleRegression();

        for(int cnt : cntCounter.keySet()) {
            reg.addData(cnt, Math.log(cntCounter.getCount(cnt)));
        }

        //System.out.println(reg.getIntercept());
        //System.out.println(reg.getSlope());
        //System.out.println(regression.getSlopeStdErr());

        double[] coeffs = new double[] {reg.getIntercept(),
                                        reg.getSlope()};

		return coeffs;
    }

	public void incrementCount(K token) {
	    lm.incrementCount(token);
	}

	public double getProb(K token) {
		if(unkTokens.contains(token) || ! lm.containsKey(token))
			return UNK_PROB;
		return lm.getCount(token);
	}

	public double totalMass() {
		return lm.totalCount();
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String test = "this this this is is a long string in which we say we say that we don't like lms";
		List<String> testToks = new ArrayList<String>(Arrays.asList(test.split("\\s+")));

		UnigramLM<String> lm = new UnigramLM<String>();
		for(String tok : testToks)
			lm.incrementCount(tok);
		lm.smoothAndNormalize();

		testToks.add("UNK");
		for(String tok : testToks) {
			double pTok = lm.getProb(tok);
			System.out.printf("%s: %.5f%n", tok, pTok);
		}
		System.out.printf("Total mass: %.3f%n",lm.totalMass());
	}
}

