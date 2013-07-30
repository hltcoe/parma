// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

// Nicholas Andrews, Johns Hopkins University
// noa@jhu.edu

package edu.jhu.hlt.parma.inference.transducers;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.Iterator;
import java.util.Arrays;

import java.lang.Character;

import java.io.FileWriter;
import java.io.FileReader;
import java.io.BufferedWriter;
import java.io.BufferedReader;
import java.io.Serializable;

import edu.jhu.hlt.parma.util.Alphabet;

import com.google.common.collect.Maps;
import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;

// NOTE: For simplicity, this class currently only assumes one feature
//       per phone.

public class ArpabetPhoneticDictionary implements PhoneticDictionary, Serializable {

    private static final long serialVersionUID = 7526472295622776147L;

    boolean IGNORE_VOWEL_STRESS = true;

    // If a dictionary has been loaded
    boolean initialized = false;

    // Encodings allowing for fast comparisons
    BiMap<String, Integer> phonemap   = HashBiMap.create(); // phone to index
    BiMap<String, Integer> featmap    = HashBiMap.create(); // feature to index
    BiMap<String, Integer> classmap   = HashBiMap.create(); // phone class to index
    Map<Integer, Integer> pclassmap = Maps.newHashMap();    // phone index to phone class index
    int [] compiled_pclassmap;

    // Alignment to phonemes for each token in the dictionary
    Map<String, Alignment> dict   = HashBiMap.create();

    Alphabet<Character> A = null;

    public ArpabetPhoneticDictionary(Alphabet<Character> a) {
        A = a;

        // === Vowels ===

        // Monophthongs
        addPhoneClass("AO", "V");
        addPhoneClass("AA", "V");
        addPhoneClass("IY", "V");
        addPhoneClass("UW", "V");
        addPhoneClass("EH", "V");
        addPhoneClass("IH", "V");
        addPhoneClass("UH", "V");
        addPhoneClass("AH", "V");
        addPhoneClass("AX", "V");
        addPhoneClass("AE", "V");

        // Diphthongs
        addPhoneClass("EY", "V");
        addPhoneClass("AY", "V");
        addPhoneClass("OW", "V");
        addPhoneClass("AW", "V");
        addPhoneClass("OY", "V");

        // R-colored vowels
        addPhoneClass("ER", "V");
        addPhoneClass("AXR", "V");
        addPhoneClass("EH R", "V");
        addPhoneClass("UH R", "V");
        addPhoneClass("AA R", "V");
        addPhoneClass("IH R", "V");
        addPhoneClass("IY R", "V");
        addPhoneClass("AW R", "V");

        // === Consonants ===

        // Stops
        addPhoneClass("P", "C");
        addPhoneClass("B", "C");
        addPhoneClass("T", "C");
        addPhoneClass("D", "C");
        addPhoneClass("K", "C");
        addPhoneClass("G", "C");

        // Affricates
        addPhoneClass("CH", "C");
        addPhoneClass("JH", "C");

        // Fricatives
        addPhoneClass("F", "C");
        addPhoneClass("V", "C");
        addPhoneClass("TH", "C");
        addPhoneClass("DH", "C");
        addPhoneClass("S", "C");
        addPhoneClass("Z", "C");
        addPhoneClass("SH", "C");
        addPhoneClass("ZH", "C");
        addPhoneClass("HH", "C");
        
        // Nasals
        addPhoneClass("M", "C");
        addPhoneClass("EM", "C");
        addPhoneClass("N", "C");
        addPhoneClass("EN", "C");
        addPhoneClass("NG", "C");
        addPhoneClass("ENG", "C");
        
        // Liquids
        addPhoneClass("L", "C");
        addPhoneClass("EL", "C");
        addPhoneClass("R", "C");
        addPhoneClass("DX", "C");
        addPhoneClass("NX", "C");

        // Semi-vowels
        addPhoneClass("Y", "C");
        addPhoneClass("W", "C");
        addPhoneClass("Q", "C");

        // Epsilon
        addPhoneClass("EPS", "NONE");

        // N/A: not in phone dictionary
        addPhoneClass("OOD", "NONE");

        compileClasses();
    }

    private void compileClasses() {
        compiled_pclassmap = new int[pclassmap.size()];
        for(int p=0;p<getNumPhones();p++) {
            compiled_pclassmap[p] = pclassmap.get(p);
        }
    }

    public boolean initialized() { return initialized; }

    private void addPhoneClass(String phone, String phoneClass) {
        int phone_index = getPhoneIndex(phone);
        int class_index = getClassIndex(phoneClass);
        pclassmap.put(phone_index, class_index);
    }

    // Load dictionary entries (including alignments)
    public void loadDictionary(String path) {

        System.err.println("Loading dictionary from: " + path);

        assert(A != null);
        
        try {
            BufferedReader br = new BufferedReader(new FileReader(path));
            String line;
            boolean in_alignment = false;
            String word = "";
            int [] xs = null;
            int [] ps = null;
            int [] cs = null;
            int cntr = 0; // count down to 0
            int i = 0;
            int line_index = 0;
            while ((line = br.readLine()) != null) {
                //                System.err.println("line = " + line.trim());
                String [] tokens = line.trim().split("\\s+");
                
                if(in_alignment) {
                    //                    System.err.println("Word: " + word);
                    String phoneme = tokens[1];
                    if(IGNORE_VOWEL_STRESS) {
                        phoneme = stripStress(phoneme);
                    }
                    // Add the alignment pair
                    String grapheme = tokens[0];
                    int x;
                    if(grapheme.equals("EPS")) {
                        x = -1;
                    } else {
                        x = A.lookupIndex(grapheme.charAt(0));
                    }
                    int p = getPhoneIndex(phoneme);
                    //                    System.err.println("phoneme = " + phoneme);
                    int c = getIntPhoneClass(p);
                    xs[i] = x;
                    ps[i] = p;
                    cs[i] = c;
                    i += 1;
                    if(--cntr == 0) in_alignment = false;
                } else {
                    if(line_index > 0) {
                        
                        assert(xs.length > 0);
                        assert(ps.length > 0);
                        assert(cs.length > 0);

                        // Save the previous alignment
                        Alignment a = new Alignment(xs, xs, ps, cs);
                        // System.err.println(word);
                        // printAlignment(a);
                        dict.put(word, a);
                    }
                    // Start a new alignment
                    word = tokens[0];
                    cntr = Integer.parseInt(tokens[1]);
                    //                    System.err.println("cntr = " + cntr);
                    xs = new int[cntr];
                    ps = new int[cntr];
                    cs = new int[cntr];
                    in_alignment = true;
                    i = 0;
                }
                line_index ++;
            }
            br.close();

            this.initialized = true;
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    private String stripStress(String phoneme) {
        int len = phoneme.length();
        Character c = phoneme.charAt(len-1);
        if(Character.isDigit(c)) {
            return phoneme.substring(0,len-1);
        }
        return phoneme;
    }

    // This is slower than it needs to be, but shouldn't matter since
    // it's usually used offline
    public Alignment getAlignment(int [] x) {
        String str = AnnotatedString.seq2str(x, A);
        return getAlignment(str);
    }

    // Alignment (of the same length as the input string) NOTE:
    // phoneme insertions are currently ignored
    public Alignment getAlignment(String input) {
        
        //        int [] xs = new int[input.length()];
        int [] xs = AnnotatedString.str2seq(input, AnnotatedString.getAlphabet());
        int [] upper_xs = new int[input.length()];
        int [] ps = new int[input.length()];
        int [] cs = new int[input.length()];

        // Set the xs and initialize the ps
        for(int i=0; i<input.length(); i++) {
            //            xs[i]       = A.lookupIndex(input.charAt(i));
            upper_xs[i] = A.lookupIndex(Character.toUpperCase(input.charAt(i)));
            ps[i]       = getPhoneIndex("OOD");
            cs[i]       = getIntPhoneClass("OOD");
        }

        // Uppercase the input
        String upper = input.toUpperCase();

        //        System.err.println("Upper: " + upper);

        for(String token : upper.split("\\s+")) {

            //            System.err.println("Token: " + token);

            // Remove punctuation
            token = token.replaceAll("\\P{L}+", "");
            if(token.length() < 3) continue;

            int tok_len = token.length();
            int start = upper.indexOf(token); // beginning on this token in the original string
            if(start < 0) continue;

            //            System.err.println("Token: " + token);

            // Is this token in the phonetic dictionary?
            if(dict.containsKey(token)) {
                Alignment ta = dict.get(token);

                               // System.err.println(ta.len());
                               // System.err.println(ta.plen());
                               // System.err.println(ta.clen());

                int j = start;
                for(int i=0; i<token.length(); i++) {
                    //                    System.err.println("i="+i);
                    ps[j] = ta.phone(i);
                    cs[j] = ta.pclass(i);
                    j ++;
                }
            }
        }

        return new Alignment(xs, upper_xs, ps, cs);
    }

    // Information on the dictionary
    public int getNumPhones() {
        return phonemap.size();
    }

    public int getNumPhoneFeatures() {
        return featmap.size();
    }

    public int getNumPhoneClasses() {
        return classmap.size();
    }

    // The given string may contain multiple tokens
    public String [] getStringPhoneSequence(String str) {
        throw new UnsupportedOperationException();
    }

    public int [] getIntPhoneSequence(String str) {
        throw new UnsupportedOperationException();
    }

    // Returns the class of the given phoneme
    public int getIntPhoneClass(String phone) {
        int index = getPhoneIndex(phone);
        return pclassmap.get(index);
    }

    public int getIntPhoneClass(int phone) {
        return pclassmap.get(phone);
    }

    public String getStrPhoneClass(String phone) {
        int index = getPhoneIndex(phone);
        return getClassName(pclassmap.get(index));
    }

    public String getStrPhoneClass(int phone) {
        return getClassName(pclassmap.get(phone));
    }

    // Return features of the given phone, e.g. stress or place of
    // articulation
    public int [] getIntPhoneFeatures(String phone) {
        throw new UnsupportedOperationException();
    }

    public int [] getIntPhoneFeatures(int phone) {
        throw new UnsupportedOperationException();
    }

    public String [] getStrPhoneFeatures(String phone) {
        throw new UnsupportedOperationException();
    }

    public String [] getStrPhoneFeatures(int phone) {
        throw new UnsupportedOperationException();
    }

    // Go between string and int representations of phones and their
    // features
    public int getPhoneIndex(String phone) {
        if(phonemap.containsKey(phone)) {
            return phonemap.get(phone);
        }

        int index = phonemap.size();
        phonemap.put(phone, index);
        return index;
    }

    public String getPhoneName(int phone) {
        return phonemap.inverse().get(phone);
    }

    public int getFeatureIndex(String feature) {
        if(featmap.containsKey(feature)) {
            return featmap.get(feature);
        }

        int index = featmap.size();
        featmap.put(feature, index);
        return index;
    }

    public String getFeatureName(int feature) {
        return featmap.inverse().get(feature);
    }

    public int getClassIndex(String pclass) {
        //        System.err.println("pclass = " + pclass);
        if(classmap.containsKey(pclass)) {
            return classmap.get(pclass);
        }
        //        System.err.println("classmap.size() = " + classmap.size());
        int index = classmap.size();
        classmap.put(pclass, index);
        return index;
    }

    // Map<String, Integer> classmap
    public String getClassName(int pclass) {
        return classmap.inverse().get(pclass);
    }
    
    // Inspection
    public void printAlignment(Alignment a) {
        for(int i=0; i<a.len(); i++) {
            String x;
            if(a.ch(i) == -1) {
                x = "EPS";
            } else {
                x = ""+(Character)A.lookupObject(a.ch(i));
            }
            String p = getPhoneName(a.phone(i));
            String c = getClassName(a.pclass(i));
            System.err.println(x+", "+p+", "+c);
        }
    }
}