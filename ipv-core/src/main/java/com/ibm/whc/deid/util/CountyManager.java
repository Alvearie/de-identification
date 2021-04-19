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
import com.ibm.whc.deid.models.County;
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class CountyManager extends LocalizedResourceManager<County> {

  private static LogManager logger = LogManager.getInstance();

  protected static final Resources resourceType = Resource.COUNTY;

  protected CountyManager() {
    super(3200, 3200);
  }

  /**
   * Creates a new CountyManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a CountyManager instance
   * 
   * @see LocalizationManager
   */
  public static CountyManager buildCountyManager(String localizationProperty) {
    CountyManager manager = new CountyManager();

    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty).getResources(resourceType);
    for (ResourceEntry entry : resourceEntries) {

      try (InputStream inputStream = entry.createStream()) {
        String countryCode = entry.getCountryCode();

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {
            String name = line.get(0);
            String shortName = line.get(1);
            String state = line.get(2);
            String populationString = line.get(3);

            if (!name.trim().isEmpty()) {
              Integer population = null;
              if (!populationString.trim().isEmpty()) {
                try {
                  population = Integer.valueOf(populationString);
                } catch (NumberFormatException e) {
                  logger.logWarn(LogCodes.WPH1014W, populationString, "county.population");
                }
              }

              County county =
                  new County(name, countryCode, shortName, state, population, name.toUpperCase());
              manager.add(county);
              manager.add(countryCode, county);

              if (!shortName.trim().isEmpty()) {
                County countyShort = new County(name, countryCode, shortName, state, population,
                    shortName.toUpperCase());
                manager.add(countyShort);
                manager.add(countryCode, countyShort);
              }
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
