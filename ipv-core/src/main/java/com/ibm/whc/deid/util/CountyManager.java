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
import com.ibm.whc.deid.models.County;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class CountyManager extends ResourceBasedManager<County> {
  /** */
  private static final long serialVersionUID = -7331835176814737907L;

  public CountyManager(String tenantId) {
    super(tenantId, Resource.COUNTY);
  }

  @Override
  public Collection<ResourceEntry> getResources() {
    return LocalizationManager.getInstance().getResources(Resource.COUNTY);
  }

  @Override
  public Map<String, Map<String, County>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, County>> counties = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      String countryCode = entry.getCountryCode();

      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String name = line.get(0);
          String shortName = line.get(1);
          String state = line.get(2);
          Integer population = Integer.valueOf(line.get(3));

          County county = new County(name, countryCode, shortName, state, population);

          String key = name.toUpperCase();
          addToMapByLocale(counties, entry.getCountryCode(), key, county);
          addToMapByLocale(counties, getAllCountriesName(), key, county);

          String shortNameKey = shortName.toUpperCase();
          addToMapByLocale(counties, entry.getCountryCode(), shortNameKey, county);
          addToMapByLocale(counties, getAllCountriesName(), shortNameKey, county);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return counties;
  }

  @Override
  public Collection<County> getItemList() {
    return getValues();
  }
}
