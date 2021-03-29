/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class CreditCardTypeManager extends ResourceBasedManager<String> {
  /** */
  private static final long serialVersionUID = -122866401583982944L;

  public CreditCardTypeManager(String tenantId, String localizationProperty) {
		super(tenantId, Resource.CREDIT_CARD_TYPE, localizationProperty);
  }

  @Override
  protected Collection<ResourceEntry> getResources() {
		return LocalizationManager.getInstance(localizationProperty).getResources(Resource.CREDIT_CARD_TYPE);
  }

  @Override
  protected Map<String, Map<String, String>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, String>> cctypes = new HashMap<>();

    for (ResourceEntry entry : entries) {
      try (CSVParser reader = Readers.createCSVReaderFromStream(entry.createStream())) {
        for (CSVRecord line : reader) {
          String cctype = line.get(0);
          String key = cctype.toUpperCase();
          addToMapByLocale(cctypes, entry.getCountryCode(), key, cctype);
          addToMapByLocale(cctypes, getAllCountriesName(), key, cctype);
        }
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return cctypes;
  }

  @Override
  public Collection<String> getItemList() {
    return getValues();
  }
}
