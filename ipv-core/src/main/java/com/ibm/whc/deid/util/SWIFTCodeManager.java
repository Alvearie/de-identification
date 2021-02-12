/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.Country;
import com.ibm.whc.deid.models.SWIFTCode;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class SWIFTCodeManager extends ResourceBasedManager<SWIFTCode> {
  /** */
  private static final long serialVersionUID = 8621077436877031606L;

  public SWIFTCodeManager(String tenantId) {
    super(tenantId, Resource.SWIFT);
  }

  protected static final CountryManager countryManager = new CountryManager(null);
  protected transient volatile Map<String, List<SWIFTCode>> codeByCountryMap;
  protected final SecureRandom random = new SecureRandom();

  @Override
  public void init() {
    this.codeByCountryMap = new ConcurrentHashMap<>();
  }

  /**
   * Gets code from country.
   *
   * @param code the code
   * @return the code from country
   */
  public String getCodeFromCountry(String code) {
    SWIFTCode swiftCode = getKey(code);
    if (swiftCode == null) {
      return getRandomKey();
    }

    String countryCode = code.substring(4, 6);
    List<SWIFTCode> list = codeByCountryMap.get(countryCode.toUpperCase());

    SWIFTCode randomCode = list.get(random.nextInt(list.size()));
    return randomCode.getCode();
  }

  @Override
  public Collection<ResourceEntry> getResources() {
    return LocalizationManager.getInstance().getResources(Resource.SWIFT);
  }

  @Override
  public Map<String, Map<String, SWIFTCode>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, SWIFTCode>> swiftCodeMap = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String code = line.get(0);
          String countryCode = code.substring(4, 6);

          Country country =
              countryManager.lookupCountry(countryCode, "en");
          if (country == null) {
            continue;
          }

          SWIFTCode swiftCode = new SWIFTCode(code, country);

          addToMapByLocale(swiftCodeMap, getAllCountriesName(), code.toUpperCase(), swiftCode);

          String ccKey = countryCode.toUpperCase();
          if (!codeByCountryMap.containsKey(ccKey)) {
            codeByCountryMap.put(ccKey, new ArrayList<SWIFTCode>());
          }

          codeByCountryMap.get(ccKey).add(swiftCode);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return swiftCodeMap;
  }

  @Override
  public Collection<SWIFTCode> getItemList() {
    return getValues();
  }

}
