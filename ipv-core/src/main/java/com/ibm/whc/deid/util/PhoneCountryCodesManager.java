/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.resources.KeyValueResource;
import com.ibm.whc.deid.resources.ResourceManager;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * Class that provides access to information about the telephone country calling codes 
 * known by the De-Identification service.
 * 
 * <p>
 * Instances of this class are thread-safe.
 */
public class PhoneCountryCodesManager extends ResourceManager<KeyValueResource> {

  private static final LogManager logger = LogManager.getInstance();

  protected PhoneCountryCodesManager() {
    // nothing required here
  }

  /**
   * Creates a new telephone country calling codes manager from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a telephone country calling codes manager instance
   * 
   * @see LocalizationManager
   */
  public static PhoneCountryCodesManager buildPhoneCountryCodesManager(String localizationProperty) {
    PhoneCountryCodesManager mgr = new PhoneCountryCodesManager();

    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty).getResources(Resource.PHONE_CALLING_CODES);
    
    for (ResourceEntry entry : resourceEntries) {
      try (InputStream inputStream = entry.createStream()) {

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {
            String key = line.get(0);
            String value = line.get(1);
            if (!key.isEmpty() && !value.isEmpty()) {
              mgr.add(new KeyValueResource(key, value));
            }
          }
        }
        
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH2003E,
            "Failed to load telephone country calling code list " + " for tenant ", e);
      }
    }

    return mgr;
  }
}
