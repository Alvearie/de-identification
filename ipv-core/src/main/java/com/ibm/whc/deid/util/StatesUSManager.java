/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.State;
import com.ibm.whc.deid.models.StateNameFormat;
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * Class that manages information about US states.
 */
public class StatesUSManager extends LocalizedResourceManager<State> {

  private static LogManager logger = LogManager.getInstance();

  protected static final Resources resourceType = Resource.STATES_US;

  protected StatesUSManager() {
    // nothing required here
  }

  /**
   * Creates a new StatesUSManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return an StatesUSManager instance
   * 
   * @see LocalizationManager
   */
  public static StatesUSManager buildStatesUSManager(String localizationProperty) {
    StatesUSManager manager = new StatesUSManager();

    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty).getResources(resourceType);
    for (ResourceEntry entry : resourceEntries) {

      try (InputStream inputStream = entry.createStream()) {
        String countryCode = entry.getCountryCode();

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {

            String name = line.get(0);
            String abbreviation = line.get(1);
            if (!name.trim().isEmpty() && !abbreviation.trim().isEmpty()) {

              Long population = null;
              String populationString = line.get(2);
              if (populationString != null && !populationString.trim().isEmpty()) {
                try {
                  population = Long.valueOf(populationString);
                } catch (NumberFormatException e) {
                  logger.logWarn(LogCodes.WPH1014W, populationString, "state.population");
                }
              }

              State state = new State(name, countryCode, abbreviation, population,
                  StateNameFormat.FULL_NAME, name.toUpperCase());
              manager.add(state);
              manager.add(countryCode, state);

              State stateAbbrv = new State(name, countryCode, abbreviation, population,
                  StateNameFormat.ABBREVIATION, abbreviation.toUpperCase());
              manager.add(stateAbbrv);
              manager.add(countryCode, stateAbbrv);
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
