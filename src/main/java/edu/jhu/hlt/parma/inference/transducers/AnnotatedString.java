// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

// Nicholas Andrews, Johns Hopkins University
// noa@jhu.edu

package edu.jhu.hlt.parma.inference.transducers;

import edu.jhu.hlt.parma.util.Alphabet;
import edu.jhu.hlt.parma.inference.transducers.PhoneticDictionary.Alignment;

public class AnnotatedString {
    static Alphabet<Character> A = new Alphabet<Character>();
    static PhoneticDictionary P = new ArpabetPhoneticDictionary(A);

    public static enum Language {Arabic, English, Czech, French, Urdu, Spanish, German}
    public static enum EntityType {GPE, PER, FAC, WEA, VEH, LOC, ORG}

    Language language;
    EntityType type;

    String s;
    Alignment a;

    public AnnotatedString(String s) {
        this(s, Language.English, EntityType.PER);
    }

    public AnnotatedString(String s, Language l, EntityType t) {
        this.s = s;
        this.language = l;
        this.type = t;
        
        if(P.initialized()) {
            a = P.getAlignment(s);
        } else {
            int [] glyphs       = str2seq(s, A);
            int [] upper_glyphs = str2seq(s.toUpperCase(), A);
            a = new Alignment(glyphs, upper_glyphs);
        }
    }

    public static int [] str2seq(String x, Alphabet<Character> A) {
		int [] res = new int[x.length()];
		for(int i=0;i<x.length();i++) {
            Character c = x.charAt(i);
			int index = A.lookupIndex(c);
			if(index<0) {
                System.err.println("OOV character: " + x.charAt(i));
                System.exit(1);
			}
			res[i] = index;
		}
		return res;
	}

    public static int [] str2seq(String x) {
		int [] res = new int[x.length()];
		for(int i=0;i<x.length();i++) {
            Character c = x.charAt(i);
			int index = A.lookupIndex(c);
			if(index<0) {
                System.err.println("OOV character: " + x.charAt(i));
                System.exit(1);
			}
			res[i] = index;
		}
		return res;
	}

    public static String seq2str(int [] x) {
        StringBuilder builder = new StringBuilder();
        for(int i=0; i<x.length; i++) {
            builder.append((Character)A.lookupObject(x[i]));
        }
        return builder.toString();
    }

    public static String seq2str(int [] x, Alphabet<Character> A) {
        StringBuilder builder = new StringBuilder();
        for(int i=0; i<x.length; i++) {
            builder.append(A.lookupObject(x[i]));
        }
        return builder.toString();
    }

    public int len() { return a.len(); }

    public String str() { return s; }
    public int [] glyphs() { return a.glyphs(); }
    public int [] upper_glyphs() { return a.upper_glyphs(); }
    public int [] phones() { return a.phones(); }
    public int [] pclasses() { return a.pclasses(); }

    public Character charAt(int pos) { return s.charAt(pos); }
    public int glyphAt(int pos) { return a.ch(pos); }
    public int upper_glyph(int pos) { return a.upper_ch(pos); }
    public int phoneAt(int pos) { return a.phone(pos); }
    public int classAt(int pos) { return a.pclass(pos); }

    public EntityType getType() { return type; }
    public Language getLanguage() { return language; }
    
    public static Alphabet<Character> getAlphabet() { return A; }
    public static PhoneticDictionary getPhoneticDictionary() { return P; }
    public static void setAlphabet(Alphabet<Character> A) { AnnotatedString.A = A; AnnotatedString.P = new ArpabetPhoneticDictionary(A); }
    public static void loadPhoneticDictionary(String path) { P.loadDictionary(path); }
}