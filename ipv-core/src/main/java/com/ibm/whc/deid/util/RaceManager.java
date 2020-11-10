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
import com.ibm.whc.deid.models.Race;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class RaceManager extends ResourceBasedManager<Race> {
  /** */
  private static final long serialVersionUID = 3518587195772769899L;

  public RaceManager(String tenantId) {
    super(tenantId, Resource.RACE_ETHNICITY);
  }

  @Override
  public Collection<ResourceEntry> getResources() {
    return LocalizationManager.getInstance().getResources(Resource.RACE_ETHNICITY);
  }

  @Override
  public Map<String, Map<String, Race>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, Race>> races = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      String countryCode = entry.getCountryCode();

      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String name = line.get(0);
          String key = name.toUpperCase();

          Race race = new Race(name, countryCode);
          addToMapByLocale(races, entry.getCountryCode(), key, race);
          addToMapByLocale(races, getAllCountriesName(), key, race);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return races;
  }

  @Override
  public Collection<Race> getItemList() {
    return getValues();
  }
}
