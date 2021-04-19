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
import com.ibm.whc.deid.models.LastName;
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * Class that provides access to known family names by localization code.
 * 
 * <p>
 * Instances of this class are thread-safe.
 */
public class NameLastManager extends LocalizedResourceManager<LastName> {

  private static final LogManager logger = LogManager.getInstance();

  protected NameLastManager() {
    super(163000, 163000);
  }

  /**
   * Creates a new NameLastManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a resource manager instance
   * 
   * @see LocalizationManager
   */
  public static NameLastManager buildNameLastManager(String localizationProperty) {
    NameLastManager nameLastManager = new NameLastManager();

    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty).getResources(Resource.LAST_NAME);
    for (ResourceEntry entry : resourceEntries) {

      try (InputStream inputStream = entry.createStream()) {
        String countryCode = entry.getCountryCode();

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {
            String name = line.get(0);
            if (!name.isEmpty()) {
              LastName nameResource = new LastName(name, countryCode);
              nameLastManager.add(nameResource);
              nameLastManager.add(countryCode, nameResource);
            }
          }
        }
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return nameLastManager;
  }
}
