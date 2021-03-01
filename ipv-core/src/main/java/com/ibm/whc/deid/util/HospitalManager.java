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

import com.ibm.whc.deid.models.Hospital;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class HospitalManager extends ResourceBasedManager<Hospital> {
  /** */
  private static final long serialVersionUID = 7945071777848441928L;

  public HospitalManager(String tenantId, String localizationProperty) {
    super(tenantId, Resource.HOSPITAL_NAMES, localizationProperty);
  }

  @Override
  public Collection<ResourceEntry> getResources() {
		return LocalizationManager.getInstance(localizationProperty).getResources(Resource.HOSPITAL_NAMES);
  }

  @Override
  public Map<String, Map<String, Hospital>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, Hospital>> hospitals = new HashMap<>();

    for (final ResourceEntry entry : entries) {
      try (final InputStream inputStream = entry.createStream();
          final CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        final String countryCode = entry.getCountryCode();

        for (CSVRecord line : reader) {
          String name = line.get(0).trim();
          Hospital hospital = new Hospital(name, countryCode);

          addToMapByLocale(hospitals, entry.getCountryCode(), name.toUpperCase(), hospital);
          addToMapByLocale(hospitals, getAllCountriesName(), name.toUpperCase(), hospital);
        }
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return hospitals;
  }

  @Override
  public Collection<Hospital> getItemList() {
    return getValues();
  }
}
