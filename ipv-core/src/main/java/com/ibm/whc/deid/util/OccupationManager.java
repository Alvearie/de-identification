/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.Occupation;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class OccupationManager extends ResourceBasedManager<Occupation> {
  /** */
  private static final long serialVersionUID = 1820857985982718471L;

  public OccupationManager(String tenantId, String localizationProperty) {
    super(tenantId, Resource.OCCUPATION, localizationProperty);
  }

  @Override
  public Collection<ResourceEntry> getResources() {
    return LocalizationManager.getInstance(LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES).getResources(Resource.OCCUPATION);
  }

  @Override
  public Map<String, Map<String, Occupation>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, Occupation>> occupations = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      String countryCode = entry.getCountryCode();
      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String occupationName = line.get(0);
          List<String> categories = new ArrayList<>();
          for (int i = 1; i < line.size(); i++) {
            String category = line.get(i);
            categories.add(category);
          }

          Occupation occupation = new Occupation(occupationName, countryCode, categories);
          String key = occupationName.toUpperCase();
          addToMapByLocale(occupations, entry.getCountryCode(), key, occupation);
          addToMapByLocale(occupations, getAllCountriesName(), key, occupation);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return occupations;
  }

  @Override
  public Collection<Occupation> getItemList() {
    return getValues();
  }
}
