/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.FirstName;
import com.ibm.whc.deid.models.Gender;
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * Class that provides access to known first names of females by localization code.
 * 
 * <p>
 * Instances of this class are thread-safe.
 */
public class NameFirstFemaleManager extends LocalizedResourceManager<FirstName> {

  private static final LogManager logger = LogManager.getInstance();

  protected NameFirstFemaleManager() {
    // nothing required here
  }

  /**
   * Creates a new instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a resource manager instance
   * 
   * @see LocalizationManager
   */
  public static NameFirstFemaleManager buildNameFirstFemaleManager(String localizationProperty) {
    NameFirstFemaleManager manager = new NameFirstFemaleManager();

    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty).getResources(Resource.FIRST_NAME_FEMALE);
    for (ResourceEntry entry : resourceEntries) {

      try (InputStream inputStream = entry.createStream()) {
        String countryCode = entry.getCountryCode();

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {
            String name = line.get(0);
            if (!name.isEmpty()) {
              FirstName nameResource = new FirstName(name, countryCode, Gender.FEMALE);
              manager.add(nameResource);
              manager.add(countryCode, nameResource);
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
