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
import com.ibm.whc.deid.resources.ResourceManager;
import com.ibm.whc.deid.resources.StringResource;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class ATCManager extends ResourceManager<StringResource> {

  private static final LogManager logger = LogManager.getInstance();

  protected ATCManager() {
    super(3700);
  }

  /**
   * Creates a new ATCManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return an ATCManager instance
   * 
   * @see LocalizationManager
   */
  public static ATCManager buildATCManager(String localizationProperty) {
    ATCManager atcManager = new ATCManager();

    try {
      Collection<ResourceEntry> resourceEntries =
          LocalizationManager.getInstance(localizationProperty).getResources(Resource.ATC_CODES);
      for (ResourceEntry entry : resourceEntries) {
        try (InputStream inputStream = entry.createStream()) {

          try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
            for (CSVRecord line : reader) {
              String code = line.get(0);
              if (!code.isEmpty()) {
                StringResource resource = new StringResource(code);
                atcManager.add(resource);
              }
            }
          }
        }
      }
    } catch (IOException e) {
      logger.logError(LogCodes.WPH1013E, e);
      throw new RuntimeException(e);
    }

    return atcManager;
  }
}
