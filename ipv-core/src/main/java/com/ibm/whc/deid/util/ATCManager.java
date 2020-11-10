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
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class ATCManager extends ResourceBasedManager<String> {
  /** */
  private static final long serialVersionUID = -4026265397294623506L;

  public ATCManager(String tenantId) {
    super(tenantId, Resource.ATC_CODES);
  }

  protected List<String> codeList;

  @Override
  public void init() {
    codeList = new ArrayList<>();
  }

  @Override
  public Collection<ResourceEntry> getResources() {
    return LocalizationManager.getInstance().getResources(Resource.ATC_CODES);
  }

  @Override
  public Map<String, Map<String, String>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, String>> codes = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String code = line.get(0);
          addToMapByLocale(codes, getAllCountriesName(), code.toUpperCase(), code);
          codeList.add(code.toUpperCase());
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return codes;
  }

  @Override
  public Collection<String> getItemList() {
    return codeList;
  }
}
