/*
 * (C) Copyright IBM Corp. 2016,2021
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

import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import com.ibm.whc.deid.models.Country;
import com.ibm.whc.deid.models.SWIFTCode;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class SWIFTCodeManager extends ResourceBasedManager<SWIFTCode> {

  private static final long serialVersionUID = 8621077436877031606L;

  protected CountryManager countryManager;
  protected Map<String, List<SWIFTCode>> codeByCountryMap;

  protected final SecureRandom random = new SecureRandom();

  public SWIFTCodeManager(String tenantId, String localizationProperty) {
    super(tenantId, Resource.SWIFT, localizationProperty);
  }

  @Override
  protected void init() {
    this.codeByCountryMap = new HashMap<>();
    this.countryManager = (CountryManager) ManagerFactory.getInstance().getManager(tenantId,
        Resource.COUNTRY, null, localizationProperty);
  }

  @Override
  protected Collection<ResourceEntry> getResources() {
    return LocalizationManager.getInstance(localizationProperty).getResources(Resource.SWIFT);
  }

  @Override
  protected Map<String, Map<String, SWIFTCode>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, SWIFTCode>> swiftCodeMap = new HashMap<>();

    for (ResourceEntry entry : entries) {
      try (InputStream inputStream = entry.createStream()) {
        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {
            String code = line.get(0);
            String countryCode = code.substring(4, 6);

            Country country = countryManager.lookupCountry(countryCode, "en");
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
        }
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

  /**
   * Retrieves a random SWIFT code value from the known SWIFT codes from the given country.
   *
   * @param code the SWIFT country code
   * 
   * @return a random code value from the indicated country or <i>null</i> if no such codes are
   *         available
   */
  public String getRandomValueFromCountry(String countryCode) {
    String code = null;
    if (countryCode != null) {
      List<SWIFTCode> list = codeByCountryMap.get(countryCode.toUpperCase());
      if (list != null && !list.isEmpty()) {
        SWIFTCode randomCode = list.get(random.nextInt(list.size()));
        code = randomCode.getCode();
      }
    }
    return code;
  }
}
