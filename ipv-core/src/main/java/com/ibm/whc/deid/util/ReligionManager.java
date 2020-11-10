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
import com.ibm.whc.deid.models.Religion;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class ReligionManager extends ResourceBasedManager<Religion> {
  /** */
  private static final long serialVersionUID = -2918967366667277711L;

  public ReligionManager(String tenantId) {
    super(tenantId, Resource.RELIGION);
  }

  @Override
  public Collection<ResourceEntry> getResources() {
    return LocalizationManager.getInstance().getResources(Resource.RELIGION);
  }

  @Override
  public Map<String, Map<String, Religion>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, Religion>> religions = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      String countryCode = entry.getCountryCode();

      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String name = line.get(0);
          String key = name.toUpperCase();

          Religion religion = new Religion(name, countryCode);
          addToMapByLocale(religions, entry.getCountryCode(), key, religion);
          addToMapByLocale(religions, getAllCountriesName(), key, religion);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return religions;
  }

  @Override
  public Collection<Religion> getItemList() {
    return getValues();
  }
}
