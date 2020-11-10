/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.MaritalStatus;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class MaritalStatusManager extends ResourceBasedManager<MaritalStatus>
    implements Serializable {
  public MaritalStatusManager(String tenantId) {
    super(tenantId, Resource.MARITAL_STATUS);
  }

  /** */
  private static final long serialVersionUID = -8386476906766371438L;

  private List<MaritalStatus> statusList;

  @Override
  public void init() {
    statusList = new ArrayList<>();
  }

  @Override
  public Collection<MaritalStatus> getItemList() {
    return statusList;
  }

  @Override
  public Collection<ResourceEntry> getResources() {
    return LocalizationManager.getInstance().getResources(Resource.MARITAL_STATUS);
  }

  @Override
  public Map<String, Map<String, MaritalStatus>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, MaritalStatus>> statuses = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      String countryCode = entry.getCountryCode();

      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord record : reader) {
          String status = record.get(0);
          String key = status.toUpperCase();

          MaritalStatus maritalStatus = new MaritalStatus(status, countryCode);

          addToMapByLocale(statuses, entry.getCountryCode(), key, maritalStatus);
          addToMapByLocale(statuses, getAllCountriesName(), key, maritalStatus);
          statusList.add(maritalStatus);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return statuses;
  }

}
