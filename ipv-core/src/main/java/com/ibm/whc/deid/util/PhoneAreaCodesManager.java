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
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * Class that provides access to information about the telephone area codes known by the
 * De-Identification service.
 * 
 * <p>
 * Each area code is represented by a resource object. The key of the resource is the area code
 * itself.
 * 
 * <p>
 * Instances of this class are thread-safe.
 */
public class PhoneAreaCodesManager extends LocalizedResourceManager<PhoneAreaCodeResource> {

  private static final LogManager logger = LogManager.getInstance();

  protected PhoneAreaCodesManager() {
    // nothing required here
  }

  /**
   * Creates a new telephone area codes manager from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a telephone area codes manager instance
   * 
   * @see LocalizationManager
   */
  public static PhoneAreaCodesManager buildPhoneAreaCodesManager(String localizationProperty) {
    PhoneAreaCodesManager mgr = new PhoneAreaCodesManager();

    Collection<ResourceEntry> resourceEntries = LocalizationManager
        .getInstance(localizationProperty).getResources(Resource.PHONE_AREA_CODES);

    for (ResourceEntry entry : resourceEntries) {
      try (InputStream inputStream = entry.createStream()) {

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {
            String country = line.get(0);
            String areaCode = line.get(1);
            if (!country.isEmpty() && !areaCode.isEmpty()) {
              PhoneAreaCodeResource resource = new PhoneAreaCodeResource(areaCode);
              // not currently used from the "all-resources" list, but add for completeness
              mgr.add(resource);
              mgr.add(country, resource);
            }
          }
        }

      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH2003E,
            "Failed to load telephone area code list " + " for tenant ", e);
      }
    }

    return mgr;
  }
}
