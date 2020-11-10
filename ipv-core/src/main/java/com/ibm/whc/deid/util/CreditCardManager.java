/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.security.SecureRandom;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.CreditCard;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class CreditCardManager {
  private final SecureRandom random;
  private Map<String, CreditCard> creditCardMap;
  private Map<String, CreditCard> creditCardMapByPrefix;
  private String[] creditCardNames;

  private static final LogManager logger = LogManager.getInstance();

  /** Instantiates a new Credit card manager. */
  public CreditCardManager() {
    this.random = new SecureRandom();

    this.creditCardMapByPrefix = new HashMap<>();
    this.creditCardMap = readResourceList();
    Set<String> keys = creditCardMap.keySet();

    this.creditCardNames = new String[keys.size()];
    creditCardNames = keys.toArray(creditCardNames);
  }

  /**
   * Read resource list map.
   *
   * @return the map
   */
  public Map<String, CreditCard> readResourceList() {
    Collection<ResourceEntry> resources =
        LocalizationManager.getInstance().getResources(Resource.CREDIT_CARD_TYPE);
    Map<String, CreditCard> ccMap = new HashMap<>();

    for (ResourceEntry resourceEntry : resources) {
      InputStream inputStream = resourceEntry.createStream();
      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String name = line.get(0);
          String prefixesEncoded = line.get(1);
          int minimumLength = Integer.parseInt(line.get(2));
          int maximumLength = Integer.parseInt(line.get(3));
          String[] prefixes = prefixesEncoded.split(":");

          CreditCard creditCard = new CreditCard(name, prefixes, minimumLength, maximumLength);
          ccMap.put(name.toUpperCase(), creditCard);

          for (int k = 0; k < prefixes.length; k++) {
            creditCardMapByPrefix.put(prefixes[k], creditCard);
          }
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return ccMap;
  }

  /**
   * Lookup info credit card.
   *
   * @param cc the cc
   * @return the credit card
   */
  public CreditCard lookupInfo(String cc) {
    if (cc.length() < 6) {
      return null;
    }

    String issuer = cc.substring(0, 6);

    for (int i = 2; i < 4; i++) {
      String prefix = issuer.substring(0, i);
      CreditCard res = creditCardMapByPrefix.get(prefix);
      if (res != null) {
        return res;
      }
    }
    return null;
  }

  /**
   * Random credit card information credit card.
   *
   * @return the credit card
   */
  public CreditCard randomCreditCardInformation() {
    int length = creditCardNames.length;
    int index = random.nextInt(length);
    String randomName = creditCardNames[index];
    return creditCardMap.get(randomName);
  }
}
