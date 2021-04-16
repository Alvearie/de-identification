/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.Occupation;
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class OccupationManager extends LocalizedResourceManager<Occupation> {

  private static LogManager logger = LogManager.getInstance();

  protected static final Resources resourceType = Resource.OCCUPATION;

  protected OccupationManager() {
    // nothing required here
  }

  /**
   * Creates a new OccupationManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return an OccupationManager instance
   * 
   * @see LocalizationManager
   */
  public static OccupationManager buildOccupationManager(String localizationProperty) {
    OccupationManager manager = new OccupationManager();

    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty).getResources(resourceType);
    for (ResourceEntry entry : resourceEntries) {

      try (InputStream inputStream = entry.createStream()) {
        String countryCode = entry.getCountryCode();

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {

            String occupationName = line.get(0);
            if (!occupationName.trim().isEmpty()) {
              int count = line.size();
              List<String> categories = new ArrayList<>(count - 1);
              for (int i = 1; i < count; i++) {
                String category = line.get(i);
                if (!category.trim().isEmpty()) {
                  categories.add(category);
                }
              }

              Occupation occupation = new Occupation(occupationName, countryCode, categories);
              manager.add(occupation);
              manager.add(countryCode, occupation);
            }
          }
        }

      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return manager;
  }
}
