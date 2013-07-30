// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

// Nicholas Andrews, Johns Hopkins University
// noa@jhu.edu

package edu.jhu.hlt.parma.inference.transducers;

/**
 * A basic reasonable model of p(output string | input string), trainable with EM.
 * The current implementation doesn't use log probabilities, so it's only appropriate 
 * for short strings.
 * 
 * Does not require any other classes.
 * Currently implements the (ugly) string transducer interface.
 *
 * - The model uses one character of lookahead to choose the next
 *   edit operation, including its output character in the case of
 *   INS or SUB operations.  This is done using a standard kind of
 *   chain rule with backoff.
 * 
 * - Boundary conditions are handled appropriately, using an EOS symbol.
 *  
 * - There is something like an affine gap penalty.  More precisely,
 *   we treat the alignment as interspersing "EDIT" with "NOEDIT"
 *   regions.  An EDIT region allows all the usual edits (including COPY,
 *   which is important for getting alignments right during an edit region),
 *   whereas a NOEDIT region just does mandatory copying  (distinct from
 *   the COPY operation).  We learn two free parameters, which effectively
 *   model the lengths of these two kinds of regions using exponential models.
 *   Formally, before choosing an operation, we first decide whether it
 *   will be NOEDIT or some kind of EDIT; this choice is conditioned only
 *   on whether the previous operation was NOEDIT or EDIT.  (At the
 *   start of the string, the previous operation is considered to be NOEDIT,
 *   and the final operation that copies EOS must be NOEDIT, so every
 *   edit region pays a cost to get in and a cost to get out, as well as
 *   a typically lesser cost at each character to stay in the region.
 * 
 * - The input string can be null, so that this class can also be used
 *   as a language model that has tied parameters with the conditional
 *   edit model.  An input of null is different from the empty string
 *   because in this case, we use a special EOS' symbol which is seen
 *   by the single character of lookahead.  This lets us learn that
 *   insertions are more likely when conditioning on null than when
 *   conditioning on an ordinary string.
 *
 * !!! TODO: The choice between EDIT and NOEDIT should probably depend
 * on whether the next character is EOS, EOS', or something else.  At
 * present it depends on nothing.  So if we like to have edit
 * sequences persist in general, then we like to insert a lot at the
 * end, too, since that's the only way to persist in an edit region at
 * the end.  Here's my suggestion: at the end when the only choices
 * are INS and NOEDIT, the backoff probability of INS (given the
 * previous state) should be taken to be only as large as it was in
 * the 5-choice world, so that (according to backoff probabilities)
 * the expected length of an insertion sequence at the final position
 * is the same as at any other position.  So the backoff mass of COPY,
 * SUB, and DEL should be transferred to NOEDIT, not to INS!
 * Furthermore, we should be able to condition on the next character
 * (perhaps EOS') to decide whether to INS or NOEDIT in this context:
 * so NOEDIT now has some lookahead.  (At present, it doesn't, so EOS
 * and EOS' are actually treated differently and x=null behaves just
 * like x="".)  An alternative way of doing this would be to also
 * transfer the foreground mass of COPY, SUB, and DEL to NOEDIT.  Then
 * we have some nondeterminism about which one was used on the final
 * character.  They have lookahead, so we can learn that they are
 * especially likely or unlikely in this context, even though the
 * regular NOEDIT is not.
 *
 * !!! TODO: Simplify the presentation and code by talking simply
 * about 5 kinds of edits (COPY,SUB,INS,DEL,NOEDIT)?  In the code here,
 * I've gone to some trouble to factor together the work on the
 * different edit operations, by using PREEDIT and PRENOEDIT states;
 * but instead we could just have the EditModel class handle
 * p(chy,op|chx,prevop==NOEDIT), which would be a *cached* product of
 * 3 probabilities.
 * 
 * !!! TODO: Add a method to show the Viterbi alignment and its edit probabilities.
 * !!! TODO: Add a method to print parameters
 * 
 * !!! TODO: Perhaps convert backoff to MacKay & Peto style hierarchical 
 * Dirichlet.
 * 
 * @author Jason Eisner, Johns Hopkins University
 * @author Nicholas Andrews, Johns Hopkins University
 */

import java.util.List;
import java.util.LinkedList;
import java.text.DecimalFormat;
import java.io.Serializable;
import java.nio.ByteBuffer;

import org.apache.commons.io.output.ByteArrayOutputStream;

import edu.jhu.hlt.parma.inference.transducers.AnnotatedString;
import edu.jhu.hlt.parma.util.Alphabet;

public class BackoffConditionalEditModel implements StringEditModel, Serializable {

    private static final long serialVersionUID = 7526472295622776147L;

    // Size of input and output alphabets.
    // E.g., the input alphabet characters are represented by integers
    // 0..sizeInAlph-1, with sizeInAlph and sizeInAlph+1 representing EOS and EOS'.
    protected int sizeInAlph;
    protected int sizeOutAlph;

    protected Alphabet<Character> X;
    protected Alphabet<Character> Y;
    
    // State of the automaton.
    // Would prefer to use properly typed enums here,
    // but inconvenient to use them as array indices.
    final static int NOEDIT = 0;    // state if previous action was a no-edit
    final static int EDIT = 1;      // state if previous action was some kind of edit (perhaps COPY)
    final static int PRENOEDIT = 2; // state if next action will be a no-edit
    final static int PREEDIT  = 3;  // state if next action will be some kind of edit (perhaps COPY)
    
    // Edit actions.  
    // Would prefer to use properly typed enums here,
    // but inconvenient to use them as array indices.
    final static int SUB = 0;
    final static int INS = 1;
    final static int COPY = 2;
    final static int DEL = 3;
    
    // Probability models for state transitions and edits.
    protected StateModel stateModel;
    protected EditModel  editModel;

    // Limits on EM iterations for training.
    final static int MAX_EM_ITER = 50;
    final static int MIN_EM_ITER = 25;
    
    /** Constructor.  Only requires size of input and output alphabets.
     * The initial parameters before training are set to "something reasonable," 
     * as are the backoff smoothing parameters, which are not trained.  We could 
     * build another constructor that lets the user specify these.
     */ 
    public BackoffConditionalEditModel(int sizeInAlph,
                                       int sizeOutAlph) {
        this.sizeInAlph = sizeInAlph;
        this.sizeOutAlph = sizeOutAlph;
        stateModel = new StateModel();
        editModel = new EditModel(sizeInAlph,sizeOutAlph);
    }

    public BackoffConditionalEditModel(int sizeInAlph,
                                       int sizeOutAlph,
                                       boolean baseline) {

        this.sizeInAlph = sizeInAlph;
        this.sizeOutAlph = sizeOutAlph;
        stateModel = new StateModel();
        editModel = new EditModel(sizeInAlph,sizeOutAlph);
        if(baseline) {
            System.err.println("=================USING BASELINE EDIT MODEL================");
            stateModel.p[0][0] = 0.00001;
            stateModel.p[0][1] = 0.00001;
            stateModel.p[1][0] = 0.99999;
            stateModel.p[1][1] = 0.99999;
            editModel.lambda = 1e10;
        }
    }

    public void setAlphabet(Alphabet<Character> A) {
        this.X = A;
        this.Y = A;
    }

    public Alphabet<Character> getAlphabet() {
        return this.X;
    }

    public void train(AnnotatedString [] xs, AnnotatedString [] ys, int restart) {
        double [] weights = new double[xs.length];
        for(int i=0;i<xs.length;i++) weights[i] = 1.0;
        train(xs,ys,weights,restart);
    }
    
    public void train(AnnotatedString [] xs, AnnotatedString [] ys, double [] weights, int nrestart) {
        assert(xs.length == ys.length);
        assert(ys.length == weights.length);
        assert(nrestart > 0);
      
        boolean converged = false;
        double ll = Double.NEGATIVE_INFINITY, prevll = Double.NEGATIVE_INFINITY;
        int iter = 0;
        System.err.println("running EM...");
        while(!converged && iter<MIN_EM_ITER) {
            ll = em_step(xs,ys,weights);
            System.err.println("Iter " + iter + ": LL="+ll);
            if(1.0 - (ll/prevll) < 0.0001) {
                converged = true;
            }
            prevll = ll;
            iter++;
            if(iter>MAX_EM_ITER) break;
        }
    } 

    public void train(AnnotatedString [] train_xs, AnnotatedString [] train_ys, double [] train_weights,
                      AnnotatedString [] test_xs,  AnnotatedString [] test_ys, double [] test_weights,
                      int nrestart) {
        System.err.println("FIXME: Ignoring test examples");
        train(train_xs, train_ys, train_weights, nrestart);
    }

    public void train(AnnotatedString [] train_xs, AnnotatedString [] train_ys, AnnotatedString [] test_xs, AnnotatedString [] test_ys, int nrestart) {
        System.err.println("FIXME: Ignoring test examples");
        train(train_xs, train_ys, nrestart);
    }

    public void train(AnnotatedString [] train_xs, AnnotatedString [] train_ys, AnnotatedString [] test_xs, AnnotatedString [] test_ys) {
        double ll = Double.NEGATIVE_INFINITY, prevll = Double.NEGATIVE_INFINITY;
        double test_ll = Double.NEGATIVE_INFINITY, prev_test_ll = Double.NEGATIVE_INFINITY;
        DecimalFormat df = new DecimalFormat("#.#");
        int iter = 0;
        do {
            prevll = ll;
            prev_test_ll = test_ll;
            ll = em_step(train_xs, train_ys);
            test_ll = calc_ll(test_xs, test_ys);
            System.err.println("===== EM iter " + iter + ": LL="+df.format(ll)+ " TEST_LL="+df.format(test_ll)+" =====");
        } while(test_ll > prev_test_ll && ++iter < MAX_EM_ITER);
    }
    
    public double em_step(AnnotatedString [] xs, AnnotatedString [] ys) {
        double [] ws = new double[xs.length];
        for(int i=0; i<ws.length; i++) ws[i] = 1.0d;
        return forward_or_em_step(xs,ys,ws,false);
    }

    /** Improves the model by running a single EM step on a weighted
     * training corpus of (x,y) pairs.  Returns the log-likelihood of
     * the training corpus under the OLD version of the model.  Because
     * of smoothing, the log-likelihood is not strictly guaranteed to
     * improve from step to step (although increasing the weights will 
     * overcome smoothing).
     * 
     * The caller should probably take many EM steps, using likelihood
     * (or some other criterion) on development data to decide when to
     * stop.
     */
    public double em_step(AnnotatedString [] xs, AnnotatedString [] ys, double [] weights) {
        return forward_or_em_step(xs,ys,weights,false);
    }
    
    public double p(AnnotatedString x, AnnotatedString y) {
        AnnotatedString [] xs = new AnnotatedString [] { x };
        AnnotatedString [] ys = new AnnotatedString [] { y };
        double[] weights = { 1 };
        return forward_or_em_step(xs,ys,weights,true);
    }

    public double [] logp(AnnotatedString [] x, AnnotatedString [] y) {
        double [] lps = new double[x.length];
        for(int k=0;k<x.length;k++) {
            lps[k] = logp(x[k], y[k]);
        }
        return lps;
    }
    
    public double logp(AnnotatedString x, AnnotatedString y) {
        AnnotatedString [] xs = new AnnotatedString [] { x };
        AnnotatedString [] ys = new AnnotatedString [] { y };
        double[] weights = { 1 };
        return Math.log(forward_or_em_step(xs,ys,weights,true));
    }

    public double calc_ll(AnnotatedString [] xs, AnnotatedString [] ys) {
        double [] ws = new double[xs.length];
        for(int i=0; i<ws.length; i++) ws[i] = 1.0d;
        return calc_ll(xs, ys, ws);
    }

    public double calc_ll(AnnotatedString [] xs, AnnotatedString [] ys, double [] ws) {
        double corpus_ll = 0.0;
        for(int k=0; k<ws.length; k++) {
            AnnotatedString x = xs[k];
            AnnotatedString y = xs[k];
            double w = ws[k];
            corpus_ll += w*logp(x, y);
        }
        return corpus_ll;
    }

    protected double [][][] forward_pass(AnnotatedString x, AnnotatedString y) {
        int xlen = (x==null) ? 0 : x.len();  // length not including EOS or EOS'
        int ylen = y.len();                     
        
        double[][][] alpha = new double[4][][];  // include room for EOS symbol, and for a little sloshing over so that we don't have to do boundary tests
        for (int state=0;state<4;++state) {
            alpha[state] = new double[xlen+2][];
            for (int chx=0;chx<xlen+2;++chx) 
                alpha[state][chx] = new double[ylen+2];
        }
        
        alpha[NOEDIT][0][0] = 1;
        for (int i=0; i <= xlen; ++i) {
            for (int j=0; j <= ylen; ++j) {
                CharPair cp = new CharPair(x,y,i,j,sizeInAlph, sizeOutAlph);
                
                alpha[PRENOEDIT][i][j] += alpha[NOEDIT][i][j] * stateModel.p(NOEDIT, NOEDIT);
                alpha[PRENOEDIT][i][j] += alpha[EDIT  ][i][j] * stateModel.p(NOEDIT, EDIT);
                alpha[PREEDIT  ][i][j] += alpha[NOEDIT][i][j] * stateModel.p(  EDIT, NOEDIT);
                alpha[PREEDIT  ][i][j] += alpha[EDIT  ][i][j] * stateModel.p(  EDIT, EDIT);
                
                if (cp.equal) {
                    alpha[NOEDIT][i+1][j+1] += alpha[PRENOEDIT][i][j] * 1;
                    alpha[EDIT  ][i+1][j+1] += alpha[PREEDIT  ][i][j] * editModel.p(-1,COPY, cp.x);
                }
                alpha[EDIT][i+1][j+1] += alpha[PREEDIT][i][j] * editModel.p(cp.y,SUB, cp.x);
                alpha[EDIT][i  ][j+1] += alpha[PREEDIT][i][j] * editModel.p(cp.y,INS, cp.x);
                alpha[EDIT][i+1][j  ] += alpha[PREEDIT][i][j] * editModel.p(-1,  DEL, cp.x);
            }
        }
        // // Show the alpha table:
        // System.out.println("---- EDIT alpha ("+xlen+"*"+ylen+") ----");
        // for (int i=0; i <= xlen+1; ++i) {
        // 	for (int j=0; j <= ylen+1; ++j) {
        // 	  System.out.print(alpha[EDIT][i][j]+"\t");
        // 	}
        // 	System.out.println();
        // }
        
        return alpha;
    }
    
    // Internal workhorse.  If justz is true, then only return z from the forward algorithm
    // on the first pair.  Otherwise do an EM step and return the corpus log-probability.
    protected double forward_or_em_step(AnnotatedString [] xs, AnnotatedString [] ys, double [] weights, boolean justz) {
        double corpus_logprob = 0;   
        
        for (int k=0; k < weights.length; ++k) {  // consider each training pair in turn
            double weight = weights[k];           // weight of this training pair
            AnnotatedString x = xs[k];         // input string; could be null
            AnnotatedString y = ys[k];         // output string
            
            // REMARK: For extra speed, we could do special handling of the
            // potentially common case where x==null or x=="".  (These cases
            // are identical except for whether the lookahead character is
            // EOS or EOS'.)  These cases can be handled extra-fast because
            // the generation path given y is completely deterministic: it
            // consists of a single EDIT region consisting only of INS edits
            // (or no EDIT region at all if y==""), followed by a NOEDIT of
            // the final EOS or EOS'.  Thus, no dynamic programming algorithm 
            // is needed to compute p(y|x) or to train on it (training is
            // in effect supervised!).  However, the results would be the same.
            
            int xlen = (x==null) ? 0 : x.len();   // length not including EOS or EOS'
            int ylen = y.len();                     
            double z, z_reverse;                  // partition function computed by forward and backward passes
            
            // forward pass
            double [][][] alpha = forward_pass(x,y);
            
            z = alpha[NOEDIT][xlen+1][ylen+1];    // sum of all paths
            if (justz) return z;                  // bail out!
            
            // assert (z != 0) : "Model was unable to explain a pair of training inputs (forward algorithm found probability 0)";
            // We should be able to keep going in this case by ignoring this example.
            
            if(z==0) { 
                System.err.println("Model was unable to explain a pair (k="+k+") of training inputs (forward algorithm found probability 0)");
                continue;
            }
            
            corpus_logprob += Math.log(z);              // accumulate log-probability of corpus so we can return it at the end
            double scale = weight / z;                  // used for training below
            
            // backward pass and accumulate into forward-backward counts.
            // This code is mechanically derived from the forward pass above (ok, I did it by hand).
            
            double[][][] beta = new double[4][][];  
            for (int state=0;state<4;++state) {
                beta[state] = new double[xlen+2][];
                for (int chx=0;chx<xlen+2;++chx) 
                    beta[state][chx] = new double[ylen+2];
            }
            
            beta[NOEDIT][xlen+1][ylen+1] = 1;
            for (int i=xlen; i >= 0; --i) {
                for (int j=ylen; j >=0; --j) {
                    CharPair cp = new CharPair(x,y,i,j,sizeInAlph,sizeOutAlph);
                    
                    if (cp.equal) {
                        beta[PRENOEDIT][i][j] += 1 * beta[NOEDIT][i+1][j+1]; 
                        beta[PREEDIT  ][i][j] += editModel.p(-1,COPY, cp.x) * beta[EDIT][i+1][j+1]; 
                        editModel.count(-1,COPY,cp.x, 
                                        alpha[PREEDIT][i][j] * editModel.p(-1,COPY, cp.x) * beta[EDIT][i+1][j+1] * scale);
                    }
                    beta[PREEDIT][i][j] += editModel.p(cp.y,SUB, cp.x) * beta[EDIT][i+1][j+1];
                    beta[PREEDIT][i][j] += editModel.p(cp.y,INS, cp.x) * beta[EDIT][i  ][j+1];
                    beta[PREEDIT][i][j] += editModel.p(-1,  DEL, cp.x)     * beta[EDIT][i+1][j  ];
                    editModel.count(cp.y,SUB,cp.x, 
                                    alpha[PREEDIT][i][j] * editModel.p(cp.y,SUB, cp.x) * beta[EDIT][i+1][j+1] * scale);
                    editModel.count(cp.y,INS,cp.x,
                                    alpha[PREEDIT][i][j] * editModel.p(cp.y,INS, cp.x) * beta[EDIT][i  ][j+1] * scale);
                    editModel.count(-1,DEL,cp.x,
                                    alpha[PREEDIT][i][j] * editModel.p(-1,DEL, cp.x)     * beta[EDIT][i+1][j  ] * scale);
                    
                    beta[NOEDIT][i][j] += stateModel.p(NOEDIT, NOEDIT) * beta[PRENOEDIT][i][j];
                    beta[EDIT][i][j]   += stateModel.p(NOEDIT, EDIT)   * beta[PRENOEDIT][i][j];
                    beta[NOEDIT][i][j] += stateModel.p(  EDIT, NOEDIT) * beta[PREEDIT  ][i][j];
                    beta[EDIT][i][j]   += stateModel.p(  EDIT, EDIT)   * beta[PREEDIT  ][i][j];
                    stateModel.count(NOEDIT,NOEDIT,
                                     alpha[NOEDIT][i][j] * stateModel.p(NOEDIT, NOEDIT) * beta[PRENOEDIT][i][j] * scale);
                    stateModel.count(NOEDIT,EDIT,
                                     alpha[EDIT  ][i][j] * stateModel.p(NOEDIT, EDIT)   * beta[PRENOEDIT][i][j] * scale);
                    stateModel.count(  EDIT,NOEDIT,
                                       alpha[NOEDIT][i][j] * stateModel.p(  EDIT, NOEDIT) * beta[PREEDIT  ][i][j] * scale);
                    stateModel.count(  EDIT,EDIT,
                                       alpha[EDIT  ][i][j] * stateModel.p(  EDIT, EDIT)   * beta[PREEDIT  ][i][j] * scale);
                }
            }
            z_reverse = beta[NOEDIT][0][0];
            assert (Math.abs(z - z_reverse) < 1e-8) : "Forward probability != backward probability ("+z+" != "+z_reverse+")";
        }
        stateModel.reestimate();
        editModel.reestimate();
        return corpus_logprob;
    }

    public AnnotatedString sample(AnnotatedString input) {
        throw new UnsupportedOperationException();
    }
    
    // --------------- STATIC NESTED CLASSES ---------------- 

    /** Looking up characters in a string and checking whether they're
     * equal is a little bit annoying because of EOS and EOS' codes.
     * So this class encapsulates that stuff. If the input character
     * is not in the output alphabet, then this returns false.
     */
    public static class CharPair {
        int x,y;       // the two characters
        boolean equal; // are they considered equal?
        
        public CharPair(AnnotatedString strx, AnnotatedString stry, int i, int j, int sizeInAlph, int sizeOutAlph) {
            if (strx==null) {
                x = sizeInAlph+1;                                          // input EOS' character
                if (j >= stry.len()) { y = sizeOutAlph+1; equal = true; }  // output EOS' character
                else { y = stry.glyphAt(j); equal = false; }               // output regular character
            } else if (i >= strx.len()) {
                x = sizeInAlph;                                            // input EOS character
                if (j >= stry.len()) { y = sizeOutAlph; equal = true; }    // output EOS character
                else { y = stry.glyphAt(j); equal = false; }               // output regular character
            } else {
                x = strx.glyphAt(i);                                       // input regular character
                if (j >= stry.len()) { y = sizeOutAlph; equal = false; }   // output EOS character
                else { y = stry.glyphAt(j); equal = (x==y); }              // output regular character
            }
        }
    }
    
    /** Simple model of the probability of transitioning between
     * EDIT and NOEDIT regions.
     */
    public static class StateModel implements Serializable {

        private static final long serialVersionUID = 7526472295622776148L;

        protected double[][] p = {{0.9,0.1},
                                  {0.1,0.9}};   // transition probabilities
        protected double[][] c = {{0,  0  },{0,  0  }};   // counts 
        
        // use default constructor 
        
        /** p(newState | oldState), where the possible states are only EDIT and NOEDIT.
         * Warning: the order of arguments in the call is the opposite of that for 
         * ConditionalEditModel.p(). */
        public double p(int newState, int oldState) {
            return p[newState][oldState];
        }
    
        /** Count an observed edit for the next call to reestimate(). */
        public void count(int newState, int oldState, double weight) {
            c[newState][oldState] += weight;
        }
        
        /** Update probabilities from counts using MLE estimate (slightly smoothed back
         * to previous estimate, to avoid division by 0 if we've seen no counts). */
        public void reestimate() {
            for (int oldState=0;oldState<2;++oldState) {
                double denom=0;
                for (int newState=0;newState<2;++newState) {
                    denom += c[newState][oldState];
                }
                for (int newState=0;newState<2;++newState) {
                    p[newState][oldState] = (c[newState][oldState] + p[newState][oldState])/(denom + 1);  // compute new probability
                    c[newState][oldState] = 0;  // reset counts to get ready to accept new training
                }
            }
        }
    }

    /** Model of p(edit op | input char) * p(output char if any | edit op, input char),
     *  with appropriatebackoff in both factors.
     */
    public static class EditModel implements Serializable {

        private static final long serialVersionUID = 7526472295622776149L;

        protected int sizeInAlph;  // size of input alphabet
        protected int sizeOutAlph; // size of output alphabet
        protected double lambda;   // strength of backoff smoothing
        
        // edit probabilities
        protected double[][] pEdit;           // pEdit[op][chx] = p(op | chx)
        protected double[]   pEditBackoff;    // pEditBackoff[op] = p(op)
        protected double[]   pCharIns;        // pCharIns[chy]      = p(chy | INS)
        protected double[][] pCharSub;        // pCharSub[chy][chx] = p(chy | SUB,chx)
        protected double[]   pCharSubBackoff; // pCharSubBackoff[chy] = p(chy | SUB)
        protected double[]   pCharBackoff;    // pCharBackoff[chy] = p(chy | INS or SUB)

        // counts.  The right column is denominator estimates, so it marginalizes
        // over the first dimension of the first-column array.
        protected double[][] cEdit;           protected double[] cEditDenom;           
        protected double[]   cEditBackoff;    protected double cEditBackoffDenom;
        protected double[]   cCharIns;        protected double cCharInsDenom;
        protected double[][] cCharSub;        protected double[] cCharSubDenom;
        protected double[]   cCharSubBackoff; protected double cCharSubBackoffDenom;
        protected double[]   cCharBackoff;    protected double cCharBackoffDenom;
        
        /** A very basic constructor, which picks "reasonable" hardcoded 
         * values for the initial edit probabilities and the smoothing parameter
         * lambda.
         */
        public EditModel(int sizeInAlph, int sizeOutAlph) {
            this.sizeInAlph = sizeInAlph;
            this.sizeOutAlph = sizeOutAlph;
            this.lambda = 3;

            pEdit = new double[4][]; for (int op=0;op<4;++op) { pEdit[op] = new double[sizeInAlph+2]; }
            pEditBackoff = new double[] { 0.1,0.1,0.6,0.2 };  // {SUB,INS,COPY,DEL} initialization so that we have nonzero probs to start with
            pCharIns = new double[sizeOutAlph+2];
            pCharSub = new double[sizeOutAlph+2][]; for (int chy=0;chy<sizeOutAlph+2;++chy) { pCharSub[chy] = new double[sizeInAlph+2]; }
            pCharSubBackoff = new double[sizeOutAlph+2];
            pCharBackoff = new double[sizeOutAlph+2];
            
            resetCounts();   // set counts to 0
            reestimate();    // come up with some initial probabilities based on no data (thus, fully backed off)
        }
        
        /** Intended to be called internally.  Resets all counts to 0. */
        protected void resetCounts() {
            cEdit = new double[4][]; 
            for (int op=0;op<4;++op) { cEdit[op] = new double[sizeInAlph+2]; }
            cEditBackoff = new double[4];
            cCharIns = new double[sizeOutAlph+2];
            cCharSub = new double[sizeOutAlph+2][]; 
            for (int chy=0;chy<sizeOutAlph+2;++chy) { cCharSub[chy] = new double[sizeInAlph+2]; }
            cCharSubBackoff = new double[sizeOutAlph+2];
            cCharBackoff = new double[sizeOutAlph+2];
            
            cEditDenom = new double[sizeInAlph+2];
            cEditBackoffDenom = 0;
            cCharInsDenom = 0;
            cCharSubDenom = new double[sizeInAlph+2];
            cCharSubBackoffDenom = 0;
            cCharBackoffDenom = 0;
        }
        
        /** p(chy,op | chx).  
         * chy is ignored unless the op is INS or SUB;
         * caller conventionally specifies chy as -1 in this case.
         *
         * Warning: the order of arguments in the call is the opposite of that for 
         * ConditionalEditModel.p(). */
        public double p(int chy, int op, int chx) {
            switch(op) {
            case INS: return pEdit[op][chx] * pCharIns[chy];
            case SUB: return pEdit[op][chx] * pCharSub[chy][chx];
            case DEL: 
            case COPY:
                return pEdit[op][chx];
            default: 
                throw new RuntimeException("unexpected edit type");
            }
        }
        
        /** Count an observed edit for the next call to reestimate().
         * weight is the number of times to count it. */
        public void count(int chy, int op, int chx, double weight) {
            cEdit[op][chx] += weight;
            if (op==INS)
                cCharIns[chy] += weight;
            else if (op==SUB)
                cCharSub[chy][chx] += weight;
        }
        
        /** Update probabilities from counts, using backoff estimates. */
        public void reestimate() {
            // First fill in all the various marginalized count tables.
            for (int op=0;op<4;++op) {
                for (int chx=0;chx<sizeInAlph;++chx) {   // if chx is EOS or EOS', we have no choice of edit (it must be INS), so don't compute any probability or use the fact that we chose INS to favor INS in backoff
                    cEditDenom      [chx]  += cEdit[op][chx];
                    cEditBackoff[op]       += cEdit[op][chx];
                }
                cEditBackoffDenom      += cEditBackoff[op];
            }
            for (int chy=0;chy<sizeOutAlph+2;++chy) {
                cCharInsDenom += cCharIns[chy];
                for (int chx=0;chx<sizeInAlph+2;++chx) {
                    cCharSubDenom       [chx] += cCharSub[chy][chx];
                    cCharSubBackoff[chy]      += cCharSub[chy][chx];
                }
                cCharSubBackoffDenom += cCharSubBackoff[chy];
                cCharBackoff[chy] += cCharIns[chy];        // sums over INS,SUB
                cCharBackoff[chy] += cCharSubBackoff[chy]; // sums over INS,SUB
                cCharBackoffDenom += cCharBackoff[chy];
            }
            
            // Now compute the various probability estimates, starting
            // with the most backed-off ones.
            for (int op=0;op<4;++op) {
                // Slightly smooth this back to previous estimate, to prevent division by 0 if
                // we didn't train on anything (as in contructor).
                pEditBackoff[op] = (cEditBackoff[op] + pEditBackoff[op]) / (cEditBackoffDenom + 1);
                for (int chx=0;chx<sizeInAlph;++chx) {
                    pEdit[op][chx] = (cEdit[op][chx] + lambda*pEditBackoff[op]) / (cEditDenom[chx] + lambda);
                }
                for (int chx=sizeInAlph;chx<sizeInAlph+2;++chx) {   // special handling when input is EOS or EOS'
                    pEdit[op][chx] = (op==INS) ? 1 : 0;   // must pick INS
                }
            }
            
            for (int chy=0;chy<sizeOutAlph;++chy) {
                pCharBackoff[chy] = (cCharBackoff[chy] + lambda/sizeOutAlph) / (cCharBackoffDenom + lambda); // backs off to all non-eos chars being equally likely
                pCharIns[chy] =        (cCharIns[chy] +        lambda*pCharBackoff[chy]) / (cCharInsDenom + lambda);
                pCharSubBackoff[chy] = (cCharSubBackoff[chy] + lambda*pCharBackoff[chy]) / (cCharSubBackoffDenom + lambda);
              for (int chx=0;chx<sizeInAlph+2;++chx) {	
                  pCharSub[chy][chx] =   (cCharSub[chy][chx] + lambda*pCharSubBackoff[chy]) / (cCharSubDenom[chx] + lambda);
              }
            }
            for (int chy=sizeOutAlph;chy<sizeOutAlph+2;++chy) {
                for (int chx=0;chx<sizeInAlph+2;++chx) {
                    pCharSub[chy][chx] = 0;   // not allowed to insert or substitute an eos character
                }
            }
            
            // Do sanity check that probabilities do sum to 1.
            for (int chx=0;chx<sizeInAlph+2;++chx) {
                double sum = 0;
                sum += p(-1,DEL, chx);  
                sum += p(-1,COPY,chx);
                for (int chy=0;chy<sizeOutAlph+2;++chy) {
                    sum += p(chy,INS,chx);
                    sum += p(chy,SUB,chx);
                }
                assert (Math.abs(sum-1) < 1e-8) : "Probabilities for input char "+chx+" sum to "+sum+" rather than 1";
                //                System.err.println(sum);
            }	
            
            resetCounts();  // get ready to accept new training
        }
    }
    
    /** Unit test. */
    public static void main(String[] args) {
        // TODO
    }

}