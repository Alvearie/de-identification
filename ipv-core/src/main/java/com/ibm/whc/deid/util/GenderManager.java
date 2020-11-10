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
import com.ibm.whc.deid.models.Sex;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class GenderManager extends ResourceBasedManager<Sex> {
  /** */
  private static final long serialVersionUID = -7343046175769273053L;

  public GenderManager(String tenantId) {
    super(tenantId, Resource.GENDER);
  }

  @Override
  public Collection<ResourceEntry> getResources() {
    return LocalizationManager.getInstance().getResources(Resource.GENDER);
  }

  @Override
  public Map<String, Map<String, Sex>> readResourcesFromFile(Collection<ResourceEntry> entries) {
    Map<String, Map<String, Sex>> map = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      String countryCode = entry.getCountryCode();

      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String name = line.get(0);
          String key = name.toUpperCase();

          Sex sex = new Sex(name, countryCode);
          addToMapByLocale(map, entry.getCountryCode(), key, sex);
          addToMapByLocale(map, getAllCountriesName(), key, sex);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return map;
  }

  @Override
  public Collection<Sex> getItemList() {
    return getValues();
  }
}
