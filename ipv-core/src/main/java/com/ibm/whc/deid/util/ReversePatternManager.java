/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import com.ibm.whc.deid.models.ReversePatternGenerator;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public final class ReversePatternManager extends ResourceBasedManager<ReversePatternGenerator>
    implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 4317861826867615401L;
	private static ReversePatternManager patternManager = null;
  private HashMap<String, String> resourcePatterns = new HashMap<String, String>();
  private Map<String, Map<String, ReversePatternGenerator>> patterns =
      new HashMap<String, Map<String, ReversePatternGenerator>>();
  private static boolean testingOnly_ExceptionOnLoadProperties = false;

  private static LogManager log = LogManager.getInstance();

  public ReversePatternManager() {
    super(null, Resource.PATTERN);
  }

  /**
   * Return the singleton instance of the PatternManager
   *
   * @return The PatternManager
   */
  public static ReversePatternManager getInstance() {
    if (patternManager == null) {
      patternManager = new ReversePatternManager();
      patternManager.readResourcesFromFile(patternManager.getResources());
    }
    return patternManager;
  }

  /**
   * Get a pattern based on the regex string provided. This pattern will be cached for the life of
   * the VM. Insertion in to the map is synchronized.
   *
   * @param pattern The regex-based pattern
   * @param languageCode The country code for the pattern
   * @return The Pattern instance for generating strings.
   */
  public ReversePatternGenerator getPatternByPattern(String pattern, String languageCode) {
    languageCode = languageCode.toUpperCase();
    Map<String, ReversePatternGenerator> countryMap = patterns.get(languageCode);
    if (countryMap == null) {
      patterns.put(languageCode, new HashMap<String, ReversePatternGenerator>());
      countryMap = patterns.get(languageCode);
    }
    if (countryMap.containsKey(pattern)) {
      return patterns.get(languageCode).get(pattern);
    }
    ReversePatternGenerator p = new ReversePatternGenerator(pattern);
    countryMap.put(pattern, p);
    return p;
  }

  public ReversePatternGenerator getPatternByPattern(String pattern) {
    return getPatternByPattern(pattern, "default");
  }

  /**
   * Get a pattern from the resource manager for this localization. This pattern is case sensitive.
   *
   * @param patternName The name of the pattern in the properties file for the locale.
   * @param languageCode The country code for the desired locale.
   * @return The Pattern object for generating strings from the given pattern.
   */
  public ReversePatternGenerator getPatternByResource(String patternName, String languageCode) {
    String pattern = resourcePatterns.get(patternName);
    if (pattern != null) {
      Map<String, ReversePatternGenerator> countryMap = patterns.get(languageCode.toUpperCase());
      if (countryMap != null) {
        return countryMap.get(pattern);
      }
    }
    return null;
  }

  /** Get the collection of all pattern resource locations, for all countries and languages */
  @Override
  protected Collection<ResourceEntry> getResources() {
    return LocalizationManager.getInstance().getResources(Resource.PATTERN);
  }

  /**
   * Load the resources in to the data structures for the Pattern Manager. This will parse the
   * properties files for each country/language pair and generate a map
   */
  @Override
  protected Map<String, Map<String, ReversePatternGenerator>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    // A new map, if one does not exist in this instance
    if (resourcePatterns == null || patterns == null) {
      resourcePatterns = new HashMap<String, String>();
      patterns = new HashMap<String, Map<String, ReversePatternGenerator>>();
    }
    // Populate the map with each resource for each language / country code
    for (ResourceEntry entry : entries) {
      String countryCode = entry.getCountryCode().toUpperCase();
      try (InputStream inputStream = entry.createStream()) {
        if (testingOnly_ExceptionOnLoadProperties) {
          throw new IOException("For testing only");
        }
        Properties reader = new Properties();
        reader.load(inputStream);
        for (Entry<Object, Object> line : reader.entrySet()) {
          // Populate the map for each pattern in this resource
          String key = (String) line.getKey();
          String patternAtKey = (String) line.getValue();
          getPatternByPattern(patternAtKey, countryCode);
          resourcePatterns.put(key, patternAtKey);
        }
        inputStream.close();
      } catch (IOException e) {
        log.logError(LogCodes.WPH1013E, e);
      }
    }
    return patterns;
  }

  /** Get all possible patterns for all countries and languages. */
  @Override
  public Collection<ReversePatternGenerator> getItemList() {
    Collection<ReversePatternGenerator> returnMe = new ArrayList<ReversePatternGenerator>();
    for (Entry<String, Map<String, ReversePatternGenerator>> e : patterns.entrySet()) {
      for (Entry<String, ReversePatternGenerator> f : e.getValue().entrySet()) {
        returnMe.add(f.getValue());
      }
    }
    return returnMe;
  }

  /**
   * For testing only.
   *
   * @param value true to cause exception, otherwise false
   */
  static void setTestingOnlyExceptionOnLoadProperties(boolean value) {
    testingOnly_ExceptionOnLoadProperties = value;
    patternManager = null;
  }
}
