// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util;

import java.net.URL;
import java.io.*;
import java.util.*;
import java.util.logging.Logger;
import java.util.regex.*;
import java.util.logging.*;

/**
 * adapted from Jerboa with permission
 * @author Benjamin Van Durme
 * @author Travis Wolfe
 * 
 *         A utility wrapper around {@code java.util.Properties}, supporting type specific querying
 *         on property values, and throwing Exception when properties are not found in cases with no
 *         default value.
 *         <p>
 *         Uses System level properties if they exist, then checks the property file that was loaded
 *         into this object as backup (so command line specified properties can supersede a
 *         configuration file).
 *         <p>
 *         Contains basic functionality for referencing other system properties, meant for things
 *         like setting a root directory just once, and making other values relative to that. Syntax
 *         is: (via example)
 *         <p>
 *         ROOT = /home/joe/project Data = {ROOT}/data
 */
public class ParmaConfig {

	// list of class names implementing edu.jhu.hlt.parma.feature_interfaces.AlignmentSimilarityFunction
	public final static String SIMILARITY_FEATURES = "features";

	public final static String EXPERIMENTS = "experiments";
	public final static String PROP_DEV = "experiments.propDev";
	public final static String PROP_TRAIN = "experiments.propTrain";
	public final static String CV_FOLDS = "experiments.cv.folds";
	public final static String PARAMETER_OUTPUT_FILE = "diagnostics.parameter.outdir";

	// see: Nick <noa@jhu.edu>
	public final static String FEATURES_TRANSDUCER_MODEL_PATH = "FEATURES_TRANSDUCER_MODEL_PATH";
	public final static String FEATURES_TRANSDUCER_ALPHABET_PATH = "FEATURES_TRANSDUCER_ALPHABET_PATH";

	public static final Logger log = Logger.getLogger(ParmaConfig.class.getName());

	public static List<String> getFeatures() {
		try {
			return Arrays.asList(getStrings(SIMILARITY_FEATURES));
		} catch (IOException e) {
			throw new RuntimeException();
		}
	}

	public static File getFile(String key) {
		File deft = new File("/dev/null");
		File f = getFile(key, deft);
		if(f == deft)
			throw new RuntimeException("cannot get file for: " + key);
		return f;
	}

	/**
	 * if this loads a file other than default, it will
	 * be a valid file (i.e. it exists and is a file)
	 */
	public static File getFile(String key, File defaultFile) {
		try {
			String location = getString(key, null);
			if(location == null)
				return defaultFile;
			File f = new File(location);
		
			// this seems to be defaulting to the correct location provided
			//System.out.println("[getFile] f.canonicalPath=" + f.getCanonicalPath());
			return f;
		}
		catch(Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static File getDirectory(String key) {
		//System.out.println("[getDirectory] noAlt, key=" + key);
		File deft = new File("/");
		File f = getDirectory(key, deft);
		if(f == deft)
			throw new RuntimeException();
		return f;
	}

	/**
	 * if this loads a directory other than default, it will
	 * be a valid directory (i.e. it exists and is a directory)
	 */
	public static File getDirectory(String key, File defaultDirectory) {
		try {
			String location = getString(key, null);
			if(location == null)
				return defaultDirectory;
			File f = new File(location);

			// seems to work?
			//System.out.println("[getDirectory] f.canonicalPath=" + f.getCanonicalPath());
			return f;
		}
		catch(Exception e) {
			throw new RuntimeException(e);
		}
	}

	private static final Logger logger = Logger.getLogger(ParmaConfig.class.getName());

	static Properties properties;
	static boolean isLoaded = false;
	static Hashtable<String, String> variables;
  static String propertiesFileName = null;
  public static String getPropertiesFileName() {
    return propertiesFileName;
  }

	static Pattern variablePattern = Pattern.compile("\\{[^\\\\}]+\\}");

	private static String parsePropertyValue(String value) throws IOException {
		String group, replacement;
		Matcher m = variablePattern.matcher(value);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			group = m.group();
			group = group.substring(1, group.length() - 1);
			replacement = ParmaConfig.getString(group, null);
			if (replacement != null)
				m.appendReplacement(sb, replacement);
			else {
				logger.warning("Cannot parse property [" + value + "], as [" + group + "] does not resolve");
				return null;
			}
		}
		m.appendTail(sb);
		return sb.toString();
	}

	public static void load() throws IOException {
		String filename = System.getProperty("config.filename");
		logger.info("Loading properties file: " + filename);
		if (filename != null) {
			load(filename);
			isLoaded = true;
      propertiesFileName = filename;
		}
	}

	public static void load(String filename) throws IOException {
		logger.config("Reading ParmaProperty file [" + filename + "]");
		if (properties == null) properties = new java.util.Properties();
		properties.load(FileUtils.getReader(filename));
		isLoaded = true;
    propertiesFileName = filename;
	}

	public static double getDouble(String key) throws IOException {
		if (!isLoaded) load();

		String value = System.getProperty(key);
		if (value == null && properties != null) value = properties.getProperty(key);
		if (value == null)
			throw new IOException("Key not found in property specification: [" + key + "]");
		if ((value = parsePropertyValue(value)) == null)
			throw new IOException("Key not resolvable in property specification: [" + key + "]");

		else
			return Double.parseDouble(value);
	}

	public static double getDouble(String key, double defaultValue) throws IOException {
		if (!isLoaded) load();

		String value = System.getProperty(key);
		if (value == null && properties != null) value = properties.getProperty(key);
		if (value == null) {
			logger.config("Returning default value for " + key + " : " + defaultValue);
			return defaultValue;
		}
		if ((value = parsePropertyValue(value)) == null) {
			logger.config("Key not fully resolvable, returning default value for " + key + " : "
					+ defaultValue);
			return defaultValue;
		} else
			return Double.parseDouble(value);
	}

	public static long getLong(String key) throws IOException {
		if (!isLoaded) load();

		String value = System.getProperty(key);
		if (value == null && properties != null) value = properties.getProperty(key);
		if (value == null)
			throw new IOException("Key not found in property specification: [" + key + "]");
		if ((value = parsePropertyValue(value)) == null)
			throw new IOException("Key not resolvable in property specification: [" + key + "]");

		return Long.parseLong(value);
	}

	public static long getLong(String key, long defaultValue) throws IOException {
		if (!isLoaded) load();

		String value = System.getProperty(key);
		if (value == null && properties != null) value = properties.getProperty(key);
		if (value == null) {
			logger.config("Returning default value for " + key + " : " + defaultValue);
			return defaultValue;
		}
		if ((value = parsePropertyValue(value)) == null) {
			logger.config("Key not fully resolvable, returning default value for " + key + " : "
					+ defaultValue);
			return defaultValue;
		} else
			return Long.parseLong(value);
	}

	public static int getInt(String key) throws IOException {
		if (!isLoaded) load();

		String value = System.getProperty(key);
		if (value == null && properties != null) value = properties.getProperty(key);
		if (value == null)
			throw new IOException("Key not found in property specification: [" + key + "]");
		if ((value = parsePropertyValue(value)) == null)
			throw new IOException("Key not resolvable in property specification: [" + key + "]");

		logger.config("Returning value for " + key + " : " + value);
		return Integer.parseInt(value);
	}

	public static int getInt(String key, int defaultValue) throws IOException {
		if (!isLoaded) load();

		String value = System.getProperty(key);
		if (value == null && properties != null) value = properties.getProperty(key);
		if (value == null) {
			logger.config("Returning default value for " + key + " : " + defaultValue);
			return defaultValue;
		}
		if ((value = parsePropertyValue(value)) == null) {
			logger.config("Key not fully resolvable, returning default value for " + key + " : "
					+ defaultValue);
			return defaultValue;
		} else
			return Integer.parseInt(value);
	}

	public static String getString(String key, String defaultValue) throws IOException {
		if (!isLoaded) load();

		String value = System.getProperty(key);
		if (value == null && properties != null) value = properties.getProperty(key);
		if (value == null) {
			logger.config("Returning default value for " + key + " : " + defaultValue);
			return defaultValue;
		}
		if ((value = parsePropertyValue(value)) == null) {
			logger.config("Key not fully resolvable, returning default value for " + key + " : "
					+ defaultValue);
			return defaultValue;
		} else
			return value;
	}

	public static String getString(String key) throws IOException {
		if (!isLoaded) load();

		String value = System.getProperty(key);
		if (value == null && properties != null) value = properties.getProperty(key);
		if (value == null)
			throw new IOException("Key not found in property specification: [" + key + "]");
		if ((value = parsePropertyValue(value)) == null)
			throw new IOException("Key not resolvable in property specification: [" + key + "]");

		return value;
	}

	public static String[] getStrings(String key, String[] defaultValue) throws IOException {
		if (!isLoaded) load();

		String value = System.getProperty(key);
		if (value == null && properties != null) value = properties.getProperty(key);
		if (value == null) {
			logger.config("Returning default value for " + key + " : " + defaultValue);
			return defaultValue;
		}
		if ((value = parsePropertyValue(value)) == null) {
			logger.config("Key not fully resolvable, returning default value for " + key + " : "
					+ defaultValue);
			return defaultValue;
		} else
			return value.split("\\s");
	}

	public static String[] getStrings(String key) throws IOException {
		if (!isLoaded) load();

		String value = System.getProperty(key);
		if (value == null && properties != null) value = properties.getProperty(key);
		if (value == null)
			throw new IOException("Key not found in property specification: [" + key + "]");
		if ((value = parsePropertyValue(value)) == null)
			throw new IOException("Key not resolvable in property specification: [" + key + "]");

		return value.split("\\s");
	}

	public static boolean getBoolean(String key, boolean defaultValue) throws IOException {
		if (!isLoaded) load();

		String value = System.getProperty(key);
		if (value == null && properties != null) value = properties.getProperty(key);
		if (value == null) {
			logger.config("Returning default value for " + key + " : " + defaultValue);
			return defaultValue;
		}
		if ((value = parsePropertyValue(value)) == null) {
			logger.config("Key not fully resolvable, returning default value for " + key + " : "
					+ defaultValue);
			return defaultValue;
		} else
			return Boolean.valueOf(value);
	}

	public static boolean getBoolean(String key) throws IOException {
		if (!isLoaded) load();

		String value = System.getProperty(key);
		if (value == null && properties != null) value = properties.getProperty(key);
		if (value == null)
			throw new IOException("Key not found in property specification: [" + key + "]");
		if ((value = parsePropertyValue(value)) == null)
			throw new IOException("Key not resolvable in property specification: [" + key + "]");
		return Boolean.valueOf(value);
	}
}



