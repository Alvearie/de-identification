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
import java.util.Map;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.State;
import com.ibm.whc.deid.models.StateNameFormat;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class StatesUSManager extends ResourceBasedManager<State> {
  /** */
  private static final long serialVersionUID = -5599163280173053663L;

  public StatesUSManager(String tenantId, String localizationProperty) {
    super(tenantId, Resource.STATES_US, localizationProperty);
  }


  @Override
  public Collection<ResourceEntry> getResources() {
    return LocalizationManager.getInstance(LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES).getResources(Resource.STATES_US);
  }

  @Override
  public Map<String, Map<String, State>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, State>> states = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      String countryCode = entry.getCountryCode();

      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String name = line.get(0);
          String abbreviation = line.get(1);
          Long population = Long.valueOf(line.get(2));
          String key = name.toUpperCase();

          State state =
              new State(name, countryCode, abbreviation, population, StateNameFormat.FULL_NAME);

          addToMapByLocale(states, entry.getCountryCode(), key, state);
          addToMapByLocale(states, getAllCountriesName(), key, state);

          String abbrvKey = abbreviation.toUpperCase();
          State stateAbbrv =
              new State(name, countryCode, abbreviation, population, StateNameFormat.ABBREVIATION);
          addToMapByLocale(states, entry.getCountryCode(), abbrvKey, stateAbbrv);
          addToMapByLocale(states, getAllCountriesName(), abbrvKey, stateAbbrv);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return states;
  }

  @Override
  public Collection<State> getItemList() {
    return getValues();
  }

}
