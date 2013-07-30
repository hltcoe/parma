// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

// Nicholas Andrews, Johns Hopkins University
// noa@jhu.edu

package edu.jhu.hlt.parma.inference.transducers;

import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.*;
import java.nio.charset.Charset;
import java.io.*;
import com.google.common.collect.*;
import com.google.common.base.Charsets;
import com.google.common.io.Files;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

public class WikipediaAliasReader {

    boolean RESTRICT_PUNCTUATION = true;
    boolean DROP_NON_LATIN_NAMES = true;
    //boolean valid = input.matches("\\p{L}+");
    Pattern latin_only    = Pattern.compile("\\p{L}+");

    List<String> entity_canonical_names = Lists.newArrayList(); // the canonical name for each entity

    Map<String,List<String>> aliases     = Maps.newHashMap();   // aliases for each canonical name
    Map<String,Set<String>> name_aliases = Maps.newHashMap();   // for each name, all of its aliases

    List<String> names = Lists.newArrayList();           // name alias strings (with duplicates)
    Set<String> unique_names = Sets.newHashSet();        // all unique name strings (including canonical names and aliases)
    List<Integer> labels = Lists.newArrayList();         // entity label from Wikipedia
    Map<String,Integer> namefreq = Maps.newHashMap();    // frequency estimated against a large corpus
    int num_entities = 0;                                // number of distinct entities (lines in input file)

    List<Set<String>> clusters = Lists.newArrayList();

    Map<String,Integer> name_labels = Maps.newHashMap();

    // Entity canonical name labels
    public List<Integer> entity_labels() {
        List<Integer> labels = new ArrayList<Integer>();
        for(int i=0;i<num_entities;i++) {
            labels.add(i);
        }
        return labels;
    }

    public int numEntities() {
        return num_entities;
    }

    public static Set<String> find_cluster(String type, List<Set<String>> clusters) {
        for(Set<String> cluster : clusters) {
            if(cluster.contains(type)) {
                return cluster;
            }
        }
        // Should never get here
        return null;
    }

    // NOTE: this returns the first matching cluster;
    //       a type may in principle be in multiple 
    //       clusters
    public Set<String> find_cluster(String type) {
        for(Set<String> cluster : clusters) {
            if(cluster.contains(type)) {
                return cluster;
            }
        }
        // Should never get here
        return null;
    }

    public List<Set<String>> get_clusters() {
        return clusters;
    }
    
    // Returns all aliases of the given name
    public Set<String> given_name_aliases(String name) {
        return name_aliases.get(name);
    }
    
    // Entity canonical names
    public List<String> entity_canonical_names() {
        return entity_canonical_names;
    }

    public int get_name_entity(int index) {
        return labels.get(index);
    }

    public int get_name_label(String name) {
        if(name_labels.containsKey(name)) {
            return name_labels.get(name);
        }
        return -1;
    }
    
    // Unique canonical names
    public Set<String> canonical_names() {
        return aliases.keySet();
    }

    // Alises for each canonical name (not respecting entity boundaries)
    public List<String> aliases(String canonical_name) {
        if(aliases.containsKey(canonical_name)) {
            return aliases.get(canonical_name);
        }
        return null;
    }

    // Return a list of all aliases (with duplicates)
    public List<String> aliases() {
        return names;
    }

    // Return a set of all canonical names and aliases
    public Set<String> unique_names() {
        return unique_names;
    }

    public List<Integer> labels() {
        return labels;
    }
    
    // The main method: 
    //   1) Read the wikipedia file
    //   2) Filter the names
    //   3) Parse the content of the file into useful data structures
    public void filter_and_parse(String filename) {
        List<String> lines = read_wikipedia_file(filename);
        lines = filter_wikipedia_file(lines);
        num_entities = lines.size();
        parse_wikipedia_file(lines);
    }
    
    // Read lines from a Wikipedia name list
    public List<String> read_wikipedia_file(String filename) {
        List<String> lines = null;
        try {
            lines = Files.readLines(new File(filename), Charsets.UTF_8);
        }  catch (IOException e) {
            System.out.println("Unable to read "+filename+": "+e.getMessage());
        }
        System.err.println(lines.size() + " lines read");
        return lines;
    }
    
    // currently a no-op
    public List<String> filter_wikipedia_file(List<String> lines) {
        return lines;
    }
    
    // Parse Wikipedia file
    public void parse_wikipedia_file(List<String> lines) {
        
        int entity = 0;

        int nskipped = 0;
        int ntotal = 0;

        for(String line : lines) {

            // Each line corresponds to an entity
            String [] raw_tokens = line.split("\t");

            // Skip any blank lines
            if(raw_tokens.length == 0) {
	            //                System.err.println("Skipping line: " + line);
	            nskipped += 1;
                continue;
            }
            ntotal += 1;

            // Filter the raw tokens
            List<String> filtered_tokens = new ArrayList<String>();
            for(String t : raw_tokens) {
                if(!t.equals("") && t.length() >= 3) {
                    // If we're filtering based on character set, do that test here
                    if(DROP_NON_LATIN_NAMES) {

                        boolean matches = true;

                        for(char c : t.toCharArray()) {
                            String str = new String();
                            str += c;
                            //                            if(str.matches("\\p{L}+") 
                            if(str.matches("^\\p{ASCII}*$")
                               || str.matches("\\s") 
                               || str.matches("\\p{Punct}")) {
                                //                                System.err.println("match: " + str);
                            } else {
                                //                                System.err.println("no match: " + str);
                                matches = false;
                            }
                        }

                        if(matches) {

                            // if(NORMALIZE_PUNCTUATION) {
                            //     for(char c : t.toCharArray()) {
                            //         String str = ""+c;
                            //         if(str.matches("\\p{Punct}")) {
                                        
                            //         }
                            //     }
                            // }

                            filtered_tokens.add(t);
                        } else {
                            //                            System.err.println("Dropping bad name: " + t);
                        }
                    }
                }
            }

            if(filtered_tokens.size() <= 1) {
	            nskipped += 1;
                //                System.err.println("Skipping line: " + line);
                continue;
            }

            String [] tokens = filtered_tokens.toArray(new String[filtered_tokens.size()]);

            // Put everything in the cluster
            Set<String> cluster = Sets.newHashSet();
            for(String t : tokens) {
                cluster.add(t);
            }
            clusters.add(cluster);

            String canonical_name = tokens[0];
            entity_canonical_names.add(canonical_name);
            name_labels.put(canonical_name, entity); // FIXME: duplicates

            if(!aliases.containsKey(canonical_name)) {
                aliases.put(canonical_name,new ArrayList<String>());
            }
            
            int index = 0;
            for(int i=1;i<tokens.length;i++) {
                names.add(tokens[i]);
                name_labels.put(tokens[i], entity); // FIXME: duplicates
                labels.add(entity);
                aliases.get(canonical_name).add(tokens[i]);
                index += 1;
            }
            entity++;
            
            // Construct the name alias set for this entity
            // NOTE: This will conflate entities
            Set<String> all_entity_names = Sets.newHashSet(tokens); // includes the "self" alias
            for(String name : tokens) {
                if(name_aliases.containsKey(name)) {
                    name_aliases.get(name).addAll(all_entity_names);
                } else {
                    name_aliases.put(name, all_entity_names);
                }
            }
            
            // Construct the set of all names (titles + aliases)
            for(String t : tokens) {
                unique_names.add(t);
            }
        }
        
        assert(names.size() == labels.size());

        System.err.println(nskipped + " skipped lines of " + ntotal);
        System.err.println(entity + " entities and " + unique_names.size() + " total distinct names");
    }
    
    public boolean match(String name1, String name2) {
        return name_aliases.get(name1).contains(name2);
    }

    public int name_freq(String name) {
        if(namefreq.containsKey(name)) {
            return namefreq.get(name);
        } 
        return 1;
    }

    public void free_name_freq_hash() {
        namefreq = null;
    }

    public Map<String,Integer> get_name_distribution() {

        // The return hash
        Map<String,Integer> ret = Maps.newHashMap();

        int num_total = unique_names.size();
        int num_oov   = 0;

        for(String s : unique_names) {
            if(namefreq.containsKey(s)) {
                ret.put(s, namefreq.get(s) );
            } else {
                ret.put(s, 1);
                num_oov += 1;
            }
        }

        System.err.println("==== NAME DISTRIBUTION: " + num_oov + " / " + num_total + " = " + (double)num_oov/(double)num_total + " OOV");

        return ret;
    }
}
