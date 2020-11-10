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

public class IMEIManager extends ResourceBasedManager<String> {
  /** */
  private static final long serialVersionUID = -5860856726235946681L;

  public IMEIManager() {
    super(null, Resource.TACDB);
  }

  @Override
  protected Collection<ResourceEntry> getResources() {
    return LocalizationManager.getInstance().getResources(Resource.TACDB);
  }

  @Override
  protected Map<String, Map<String, String>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, String>> tacs = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String tac = line.get(0);
          addToMapByLocale(tacs, getAllCountriesName(), tac.toUpperCase(), tac);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return tacs;
  }

  @Override
  public Collection<String> getItemList() {
    return getValues();
  }
}
