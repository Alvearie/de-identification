/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class GeneralizeManager implements Manager {
  private final Map<String, String> generalizeValueMap; // Map for *ANY
  // category
  private final Map<String, Set<String>> generalizeCategoryMap;

  private static final Collection<ResourceEntry> resourceGeneralizeSets =
      LocalizationManager.getInstance(LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES).getResources(Resource.GENERALIZE);

  private static final LogManager logger = LogManager.getInstance();

  /** Instantiates a new GeneralizeManager for parsing and reading common generalize resource. */
  public GeneralizeManager() {
    this.generalizeValueMap = new HashMap<>();
    this.generalizeCategoryMap = new HashMap<>();
    readGeneralizeSets(resourceGeneralizeSets);
  }

  /**
   * Reads generalize resources.
   *
   * @param entries collection of resources
   */
  private void readGeneralizeSets(Collection<ResourceEntry> entries) {

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();

      // The first element of each line is the generalize category
      // followed by values for that category
      // There can be multiple lines per category
      try (CSVParser parser = Readers.createCSVReaderFromStream(inputStream, ',', '"')) {
        for (CSVRecord record : parser) {
          String category = record.get(0).trim();
          if (generalizeCategoryMap.get(category) == null) {
            generalizeCategoryMap.put(category, new HashSet<>());
          }
          for (int i = 1; i < record.size(); i++) {
            String value = record.get(i).trim();
            generalizeCategoryMap.get(category).add(value);
            // generalizeValueMap is used for *ANY category;
            // therefore,
            // if value is in multiple categories, we use the first
            // occurrence.
            if (generalizeValueMap.get(value) == null) {
              generalizeValueMap.put(value, category);
            }
          }
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }
  }

  /**
   * Return the generalize value map
   *
   * @return generalizeValueMap
   */
  public Map<String, String> getGeneralizeValueMap() {
    return generalizeValueMap;
  }

  /**
   * Return the generalize category map
   *
   * @return generalizeCategoryMap
   */
  public Map<String, Set<String>> getGeneralizeCategoryMap() {
    return generalizeCategoryMap;
  }

  @Override
  public boolean isValidKey(String identifier) {
    // TODO Auto-generated method stub
    return false;
  }

  @Override
  public String getRandomKey() {
    // TODO Auto-generated method stub
    return null;
  }
}
