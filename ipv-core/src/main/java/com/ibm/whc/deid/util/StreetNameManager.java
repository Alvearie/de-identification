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
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class StreetNameManager extends ResourceBasedManager<String> {
  /** */
  private static final long serialVersionUID = 140617576732256592L;

  private static final Collection<ResourceEntry> resourceStreetNameList =
      LocalizationManager.getInstance(LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES).getResources(Resource.STREET_NAMES);

  @Override
  public Collection<ResourceEntry> getResources() {
    return resourceStreetNameList;
  }

  @Override
  public Map<String, Map<String, String>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, String>> names = new HashMap<>();

    for (ResourceEntry entry : entries) {

      InputStream inputStream = entry.createStream();
      String countryCode = entry.getCountryCode();

      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String sname = line.get(0).trim();
          String key = sname.toUpperCase();

          addToMapByLocale(names, countryCode, key, sname);
          addToMapByLocale(names, getAllCountriesName(), key, sname);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return names;
  }

  /**
   * Instantiates a new Street name manager.
   *
   * @param tenantId tenant id
 * @param localizationProperty TODO
   */
  public StreetNameManager(String tenantId, String localizationProperty) {
    super(tenantId, Resource.STREET_NAMES, localizationProperty);
  }

  @Override
  public Collection<String> getItemList() {
    return getValues();
  }
}
