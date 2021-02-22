/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.InputStreamReader;
import java.security.SecureRandom;
import java.util.HashSet;
import java.util.Set;
import com.ibm.whc.deid.seceng.SecEngBufferedReader;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class TLDManager {
  private static TLDManager instance = new TLDManager();
  /** The Random. */
  SecureRandom random;

  private String[] tlds = {"com", "org", "edu", "co.uk"};
  private Set<String>[] tldSet = new HashSet[256];

  private static LogManager logger = LogManager.getInstance();

  private TLDManager() {
    this.random = new SecureRandom();

    for (int i = 0; i < 256; i++) {
      tldSet[i] = new HashSet<>();
    }

    buildList();
  }

  /**
   * Instance tld manager.
   *
   * @return the tld manager
   */
  public static TLDManager instance() {
    return instance;
  }

  private void buildList() {
    ResourceEntry filename = LocalizationManager.getInstance(LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES)
        .getResources(Resource.PUBLIC_SUFFIX_LIST).iterator().next();

    String line = null;
    try (SecEngBufferedReader reader =
        new SecEngBufferedReader(new InputStreamReader(filename.createStream()))){
      while ((line = reader.readLine()) != null) {
        line = line.trim();
        if (line.length() == 0 || line.startsWith("//")) {
          continue;
        }

        int index = line.charAt(0);
        if (index > 255) {
          index = 0;
        }

        tldSet[index].add(line);
      }

      reader.close();
    } catch (Exception e) {
      logger.logError(LogCodes.WPH1013E, e);
      // throw new RuntimeException("error building TLD list");
    }
  }

  /**
   * Gets tld.
   *
   * @param hostname the hostname
   * @return the tld
   */
  public String getTLD(String hostname) {
    int r = 0;
    hostname = hostname.toLowerCase();
    do {
      int index = hostname.charAt(0);
      if (index > 255) {
        index = 0;
      }
      if (tldSet[index].contains(hostname)) {
        return hostname;
      }

      r = hostname.indexOf('.', r);
      if (r != -1) {
        hostname = hostname.substring(r + 1);
      }
    } while (r > 0);

    return null;
  }

  /**
   * Gets random tld.
   *
   * @return the random tld
   */
  public String getRandomTLD() {
    return this.tlds[random.nextInt(tlds.length)];
  }

  /**
   * Gets random tld.
   *
   * @param exception the exception
   * @return the random tld
   */
  public String getRandomTLD(String exception) {
    String res = this.tlds[random.nextInt(tlds.length)];
    if (res.equals(exception)) {
      return getRandomTLD(exception);
    }

    return res;
  }
}
