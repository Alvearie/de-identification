/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.CreditCardType;
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class CreditCardTypeManager extends LocalizedResourceManager<CreditCardType> {

  private static final LogManager logger = LogManager.getInstance();

  protected CreditCardTypeManager() {
    // nothing required here
  }

  /**
   * Creates a new CreditCardTypeManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a CreditCardTypeManager instance
   * 
   * @see LocalizationManager
   */
  public static CreditCardTypeManager buildCreditCardTypeManager(String localizationProperty) {
    CreditCardTypeManager manager = new CreditCardTypeManager();

    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty)
            .getResources(Resource.CREDIT_CARD_TYPE);
    for (ResourceEntry entry : resourceEntries) {

      try (InputStream inputStream = entry.createStream()) {
        String countryCode = entry.getCountryCode();

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {
            String name = line.get(0);
            if (!name.trim().isEmpty()) {
              String prefixesEncoded = line.get(1);
              String[] prefixes = prefixesEncoded.split(":");
              int minimumLength = Integer.parseInt(line.get(2));
              int maximumLength = Integer.parseInt(line.get(3));
              CreditCardType creditCard =
                  new CreditCardType(name, prefixes, minimumLength, maximumLength);
              manager.add(creditCard);
              manager.add(countryCode, creditCard);
            }
          }
        }
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return manager;
  }
}
