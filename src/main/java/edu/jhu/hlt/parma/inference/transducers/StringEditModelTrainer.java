// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

// Nicholas Andrews, Johns Hopkins University
// noa@jhu.edu

package edu.jhu.hlt.parma.inference.transducers;

import java.util.List;
import java.util.Set;
import java.util.HashSet;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Collection;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;

import java.text.DecimalFormat;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

import edu.jhu.hlt.parma.util.Alphabet;

public class StringEditModelTrainer {

    public static boolean USE_ALL_ALIASES = false;

    // Currently not used
    public static String normalize(String s) {
        String ret = new String(s);

        // Upper-case everything
        ret = ret.toUpperCase();

        // Non-alpha numeric characters only
        ret = ret.replaceAll("[^A-Z0-9 ]", "");
        
        return ret;
    }

    private void dumpTokens(String wiki_path, String out_path) {
        WikipediaAliasReader reader = new WikipediaAliasReader();
        reader.filter_and_parse(wiki_path);
        Set<String> clean_upper_tokens = new HashSet<String>();
        for(String name : reader.unique_names()) {
            for(String token : name.split("\\s+")) {
                String no_punct = token.replaceAll("\\P{L}+", "");
                String upper = no_punct.toUpperCase();
                if(upper.length() > 2) {
                    clean_upper_tokens.add(upper);
                }
            }
        }
        System.err.println("Writing " + clean_upper_tokens.size() + " tokens to " + out_path);
        try {
            FileWriter fw = new FileWriter(new File(out_path), true);
            BufferedWriter bw = new BufferedWriter(fw);
            for(String s : clean_upper_tokens) {
                bw.write(s+"\n");
            }
            bw.close();
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    private void writeModel(StringEditModel transducer, String filename) {
        try {
            FileOutputStream out = new FileOutputStream(filename);
            ObjectOutputStream objOut = new ObjectOutputStream(out);
            objOut.writeObject(transducer);
            objOut.close();
        } catch (IOException e) {
            System.out.println(e.getMessage());
            System.exit(1);
        }
    }

    private StringEditModel readModel(String filename) {
        System.err.println("Reading model from: " + filename);
        try {
            ObjectInputStream objIn = new ObjectInputStream(new FileInputStream(filename));
            return (StringEditModel) objIn.readObject();
        } catch (IOException e) {
            System.out.println(e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }  catch(ClassNotFoundException c) {
            System.out.println(c.getMessage());
            c.printStackTrace();
            System.exit(1);
        }
        return null;
    }

    public static AnnotatedString [] fromList(List<AnnotatedString> list) {
        AnnotatedString [] arr = new AnnotatedString[list.size()];
        for(int i=0; i<list.size(); i++) {
            arr[i] = list.get(i);
        }
        return arr;
    }

    public static Collection<String> aliasAtRandom(List<String> aliases) {
        int index = RandomNumberGenerator.nextInt(aliases.size());
        Collection<String> ret = new ArrayList<String>();
        ret.add(aliases.get(index));
        return ret;
    }

    public static AnnotatedString [][] getPairs(String path, boolean flip, boolean use_all_aliases) {
        WikipediaAliasReader reader = new WikipediaAliasReader();
        reader.filter_and_parse(path);
        List<AnnotatedString> inputs  = new ArrayList<AnnotatedString>();
        List<AnnotatedString> outputs = new ArrayList<AnnotatedString>();
        for(String input : reader.canonical_names()) {
            Collection<String> aliases;
            if(!use_all_aliases) {
                aliases = aliasAtRandom(reader.aliases(input));
            } else {
                aliases = reader.aliases(input);
            }
            for(String output : aliases) {
                //                System.err.println("input: " + input + ", output: " + output);
                if(flip) {
                    inputs.add(new AnnotatedString(input));
                    outputs.add(new AnnotatedString(output));
                } else {
                    inputs.add(new AnnotatedString(output));
                    outputs.add(new AnnotatedString(input));
                }
            }
        }
        return new AnnotatedString [][] { fromList(inputs), fromList(outputs) };
    }

    public boolean execute(CommandLine cmd) {

        AnnotatedString [][] train = null;
        AnnotatedString [][] dev   = null;

        Alphabet<Character> A = null;
        PhoneticDictionary P = null;
        StringEditModel model = null;

        if(cmd.hasOption("train") && cmd.hasOption("dump")) {
            dumpTokens(cmd.getOptionValue("train"), cmd.getOptionValue("dump"));
            System.exit(0);
        }

        if(cmd.hasOption("input")) {
            model = readModel(cmd.getOptionValue("input"));
            A = model.getAlphabet();
            AnnotatedString.setAlphabet(A);
        }

        // Phonetic features
        if(cmd.hasOption("dict")) {
            System.err.println("Reading dictionary from: " + cmd.getOptionValue("dict"));
            AnnotatedString.loadPhoneticDictionary(cmd.getOptionValue("dict"));
        }

        // ===== Training =====

        if(cmd.hasOption("train") && !cmd.hasOption("model")) {
            System.err.println("Must specify model type.");
            System.exit(1);
        }

        // This will either populate a new dictionary, or use an
        // existing dictionary from a previously trained model.
        if(cmd.hasOption("train")) {
            System.err.println("Loading training pairs...");
            train = getPairs(cmd.getOptionValue("train"), cmd.hasOption("flip"), USE_ALL_ALIASES);
        }

        if(cmd.hasOption("dev")) {
            System.err.println("Loading dev pairs...");
            dev = getPairs(cmd.getOptionValue("dev"), cmd.hasOption("flip"), USE_ALL_ALIASES);
        }

        // Set the di
        if(!cmd.hasOption("input"))
            A = AnnotatedString.getAlphabet();
        
        if(cmd.hasOption("model")) {
            String type = cmd.getOptionValue("model");
            if(type.equals("loglinear")) {
                // ParallelLogLinearConditionalEditModel linear = new ParallelLogLinearConditionalEditModel(A.size(), A.size());
                // int nthreads = 1;
                // if(cmd.hasOption("threads")) {
                //     nthreads = Integer.parseInt(cmd.getOptionValue("threads"));
                // }
                // linear.setNumThreads(nthreads);
                // model = linear;
            }
            else if(type.equals("backoff")) {
                BackoffConditionalEditModel backoff = new BackoffConditionalEditModel(A.size(), A.size());
                backoff.setAlphabet(A);
                model = backoff;
            }
            else {
                System.err.println("Unknown model type: " + type);
                System.exit(1);
            }
            // Train the model
            model.train(train[0], train[1], dev[0], dev[1]);
        }

        // Stop the alphabet growth
        A.stopGrowth();

        // ===== Serialize the model =====
        if(cmd.hasOption("output")) {
            writeModel(model, cmd.getOptionValue("output"));
        }
        
        return true;
    }

    private static Options createOptions() {
		Options options = new Options();

        options.addOption("n","normalize",false,"Normalize input strings (strip punctuation, case normalize, sort)");
        options.addOption("t","train",true,"Path to training data.");
        options.addOption("d","dev",true,"Path to development data.");
        options.addOption("f","flip",false,"Flip inputs and outputs when creating training and dev pairs.");
        options.addOption("r","ranking",true,"Path to data for ranking evaluation.");
        options.addOption("m","model",true,"Model type.");
        options.addOption("i","input",true,"Path to serialized model.");
        options.addOption("o","output",true,"Path to save serialized model.");
        options.addOption("u","dump",true,"Path to dump all name tokens (to create a phonetic dictionary).");
        options.addOption("a","dict",true,"Path to phonetic dictionary.");
        options.addOption("p","threads",true,"Number of threads to use.");
        options.addOption("e","everything",false,"If all the aliases are used for each entity (or just one at random).");
        options.addOption("s","sample",false,"Run some sampling tests.");

        return options;
    }

    public static void main(String [] args) {
        
        final String usage = "java " + StringEditModelTrainer.class.getName() + " [OPTIONS]";
		final CommandLineParser parser = new PosixParser();
		final Options options = createOptions();
		CommandLine cmd = null;
		final HelpFormatter formatter = new HelpFormatter();
		try {
			cmd = parser.parse(options, args);
		} catch (ParseException e1) {
            System.err.println(e1.getMessage());
			formatter.printHelp(usage, options, true);
			System.exit(-1);
		}

		final StringEditModelTrainer trainer = new StringEditModelTrainer();
		final boolean success = trainer.execute(cmd);
		if(! success) {
			formatter.printHelp(usage, options, true);
			System.exit(-1);
		}
	}
}