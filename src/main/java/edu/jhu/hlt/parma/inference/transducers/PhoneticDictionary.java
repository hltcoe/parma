// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

/**
 * An interface for a map from words to their phonetic
 * representations.
 *
 * @author Nicholas Andrews, Johns Hopkins University
 */
package edu.jhu.hlt.parma.inference.transducers;

public interface PhoneticDictionary {

    // Load dictionary entries
    public boolean initialized();
    public void loadDictionary(String path);

    // Alignment
    public Alignment getAlignment(String input);
    public Alignment getAlignment(int [] x);

    // Information on the dictionary
    public int getNumPhones();
    public int getNumPhoneFeatures();
    public int getNumPhoneClasses();

    // The given string may contain multiple tokens
    public String [] getStringPhoneSequence(String str);
    public int    [] getIntPhoneSequence(String str);

    // Returns the class of the given phoneme
    public int     getIntPhoneClass(String phone);
    public int     getIntPhoneClass(int phone);
    public String  getStrPhoneClass(String phone);
    public String  getStrPhoneClass(int phone);

    // Return features of the given phone, e.g. stress or place of
    // articulation
    public int    [] getIntPhoneFeatures(String phone);
    public int    [] getIntPhoneFeatures(int phone);
    public String [] getStrPhoneFeatures(String phone);
    public String [] getStrPhoneFeatures(int phone);

    // Go between string and int representations of phones and their
    // features
    public int     getPhoneIndex(String phone);
    public String  getPhoneName(int phone);
    public int     getFeatureIndex(String feature);
    public String  getFeatureName(int feature);
    public int     getClassIndex(String pclass);
    public String  getClassName(int pclass);

    // Inspection
    public void printAlignment(Alignment a);

    public static class Alignment {
        int [] input;
        int [] upper_input;
        int [] phone;
        int [] phone_class;
        public Alignment(int [] input, int [] upper_input, int [] phone, int [] phone_class) {
            this.input = new int[input.length];
            this.upper_input = new int[upper_input.length];
            this.phone = new int[phone.length];
            this.phone_class = new int[phone_class.length];
            System.arraycopy(input, 0, this.input, 0, input.length);
            System.arraycopy(upper_input, 0, this.upper_input, 0, input.length);
            System.arraycopy(phone, 0, this.phone, 0, phone.length);
            System.arraycopy(phone_class, 0, this.phone_class, 0, phone_class.length);
        }
        public Alignment(int [] input, int [] input_upper) {
            setGlyphs(input);
            setUpperGlyphs(input_upper);
        }
        public void setGlyphs(int [] input) { 
            this.input = new int[input.length];
            System.arraycopy(input, 0, this.input, 0, input.length);
        }
        public void setUpperGlyphs(int [] upper_input) {
            this.upper_input = new int[input.length];
            System.arraycopy(upper_input, 0, this.upper_input, 0, input.length);
        }
        public int len() { return input.length; }
        public int plen() { return phone.length; }
        public int clen() { return phone_class.length; };

        public int [] glyphs() { return input; }
        public int [] upper_glyphs() { return upper_input; }
        public int [] phones() { return phone; }
        public int [] pclasses() { return phone_class; }

        public int ch(int pos) { return input[pos]; }
        public int upper_ch(int pos) { return upper_input[pos]; }
        public int phone(int pos) { return phone[pos]; }
        public int pclass(int pos) { return phone_class[pos]; }
    }
}