/*
 * (C) Copyright IBM Corp. 2016,2021
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
import com.ibm.whc.deid.models.Country;
import com.ibm.whc.deid.models.SWIFTCode;
import com.ibm.whc.deid.resources.ResourceManager;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * Class that provides access to information about the SWIFT codes known by the De-Identification
 * service.
 * 
 * <p>
 * Instances of this class are thread-safe.
 */
public class SWIFTCodeManager extends ResourceManager<SWIFTCode> {

  private static final LogManager logger = LogManager.getInstance();

  /**
   * SWIFT codes contain a two-character country code. Map each country code represented in the
   * resources to the list of codes for that country.
   * 
   * <p>
   * Note - this Map is only modified during construction of the instance
   */
  protected final Map<String, List<SWIFTCode>> codeByCountryMap = new HashMap<>();

  protected SWIFTCodeManager() {
    // nothing required here
  }

  /**
   * Creates a new SWIFTCodeManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a SWIFTCodeManager instance
   * 
   * @see LocalizationManager
   */
  public static SWIFTCodeManager buildSWIFTCodeManager(String localizationProperty,
      String tenantId) {
    SWIFTCodeManager swiftCodeManager = new SWIFTCodeManager();
    CountryManager countryManager = (CountryManager) ManagerFactory.getInstance()
        .getManager(tenantId, Resource.COUNTRY, null, localizationProperty);

    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty).getResources(Resource.SWIFT);
    for (ResourceEntry entry : resourceEntries) {
      try (InputStream inputStream = entry.createStream()) {

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {
            String code = line.get(0);
            swiftCodeManager.add(code, countryManager);
          }
        }
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return swiftCodeManager;
  }

  protected void add(String code, CountryManager countryManager) {
    if (code.length() >= 8) {
      String countryCode = code.substring(4, 6);

      Country country = countryManager.getValue(countryCode);
      if (country != null) {

        SWIFTCode swiftCode = new SWIFTCode(code, country);
        add(swiftCode);

        String ccKey = countryCode.toUpperCase();
        List<SWIFTCode> countryList = codeByCountryMap.get(ccKey);
        if (countryList == null) {
          countryList = new ArrayList<>();
          codeByCountryMap.put(ccKey, countryList);
        }
        countryList.add(swiftCode);
      }
    }
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
