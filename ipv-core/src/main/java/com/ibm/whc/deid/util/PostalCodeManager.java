/*
 * (C) Copyright IBM Corp. 2016,2021
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
import com.ibm.whc.deid.models.LatitudeLongitude;
import com.ibm.whc.deid.models.PostalCode;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class PostalCodeManager implements Manager, Serializable {

  private static final long serialVersionUID = -5126260477789733871L;

  protected static final Collection<ResourceEntry> resourceList =
      LocalizationManager.getInstance(LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES).getResources(Resource.POSTAL_CODES);
  protected final MapWithRandomPick<String, PostalCode> postalCodeMap;
  protected List<PostalCode> postalCodeList;
  private LatLonDistance<PostalCode> latLonTree = null;

  private static LogManager logger = LogManager.getInstance();
  protected final Resources resourceType = Resource.POSTAL_CODES;

  protected final String tenantId;

  public PostalCodeManager(String tenantId) {
    this.tenantId = tenantId;
    this.postalCodeList = new ArrayList<>();

    this.postalCodeMap = new MapWithRandomPick<>(new HashMap<String, PostalCode>());

    readResources(resourceType, tenantId);
    this.postalCodeMap.setKeyList();

    try {
      this.latLonTree = new LatLonDistance<>(postalCodeList);
    } catch (Exception e) {
      logger.logError(LogCodes.WPH1013E, e);
    }

  }

  protected void readResources(Resources resourceType, String tenantId) {
    this.postalCodeMap.getMap().putAll(readPostalCodeCodeListFromFile(resourceList));
  }

  /**
   * Read postal code list from the default CSV file
   *
   * @param entries
   * @return
   */
  protected Map<? extends String, ? extends PostalCode> readPostalCodeCodeListFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, PostalCode> postals = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      entry.getCountryCode();

      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          line.get(0);
          String code = line.get(1);
          Double latitude = FileUtils.parseDouble(line.get(2));
          Double longitude = FileUtils.parseDouble(line.get(3));
          /* TODO : replace hardcoded locale */
          PostalCode postalCode = new PostalCode(code, latitude, longitude);
          this.postalCodeList.add(postalCode);
          postals.put(code.toUpperCase(), postalCode);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return postals;
  }

  public String getPseudorandom(String identifier) {
    int position =
        (int) (Math.abs(HashUtils.longFromHash(identifier)) % this.postalCodeList.size());
    return this.postalCodeList.get(position).getName();
  }

  /**
   * Gets closest postal codes.
   *
   * @param postalCode the postal code
   * @param k the k
   * @return the closest postal codes
   */
  public List<PostalCode> getClosestPostalCodes(String postalCode, int k) {
    String key = postalCode.toUpperCase();
    PostalCode lookup = this.postalCodeMap.getMap().get(key);

    if (lookup == null) {
      return new ArrayList<>();
    }

    LatitudeLongitude latlon = lookup.getLocation();

    return this.latLonTree.findNearestK(latlon.getLatitude(), latlon.getLongitude(), k);
  }

  @Override
  public String getRandomKey() {
    return this.postalCodeMap.getRandomKey();
  }

  @Override
  public boolean isValidKey(String postalCode) {
    return postalCodeMap.getMap().containsKey(postalCode.toUpperCase());
  }

}
