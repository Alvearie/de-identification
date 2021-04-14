/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.resources.StringResource;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class StreetNameManager extends LocalizedResourceManager<StringResource> {

  private static LogManager logger = LogManager.getInstance();

  protected static final Resources resourceType = Resource.STREET_NAMES;

  protected StreetNameManager() {
    // nothing required here
  }

  /**
   * Creates a new StreetNameManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a StreetNameManager instance
   * 
   * @see LocalizationManager
   */
  public static StreetNameManager buildStreetNameManager(String localizationProperty) {
    StreetNameManager manager = new StreetNameManager();

    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty).getResources(resourceType);
    for (ResourceEntry entry : resourceEntries) {

      try (InputStream inputStream = entry.createStream()) {
        String countryCode = entry.getCountryCode();

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {
            String sname = line.get(0).trim();
            if (!sname.isEmpty()) {
              StringResource streetname = new StringResource(sname);
              manager.add(streetname);
              manager.add(countryCode, streetname);
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
