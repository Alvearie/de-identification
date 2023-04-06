/*
 * © Merative US L.P. 2021
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
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import com.ibm.whc.deid.utils.log.Messages;

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
    super(300, 300);
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

    try {
      Collection<ResourceEntry> resourceEntries = LocalizationManager
          .getInstance(localizationProperty).getResources(Resource.PHONE_AREA_CODES);

      for (ResourceEntry entry : resourceEntries) {
        try (InputStream inputStream = entry.createStream()) {
          String fileName = entry.getFilename();

          try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
            for (CSVRecord line : reader) {
              loadCSVRecord(fileName, mgr, line);
            }
          }
        }
      }

    } catch (IOException e) {
      logger.logError(LogCodes.WPH1013E, e);
      throw new RuntimeException(e);
    }

    return mgr;
  }
  
  /**
   * Retrieves data from the given Comma-Separated Values (CSV) record and loads it into the given
   * resource manager.
   *
   * @param fileName the name of the file from which the CSV data was obtained - used for logging
   *        and error messages
   * @param manager the resource manager
   * @param record a single record read from a source that provides CSV format data
   * 
   * @throws RuntimeException if any of the data in the record is invalid for its target purpose.
   */
  protected static void loadCSVRecord(String fileName, PhoneAreaCodesManager manager,
      CSVRecord record) {
    try {
      loadRecord(manager, record.get(0), record.get(1));

    } catch (RuntimeException e) {
      // CSVRecord has a very descriptive toString() implementation
      String logmsg =
          Messages.getMessage(LogCodes.WPH1023E, String.valueOf(record), fileName, e.getMessage());
      throw new KeyedRuntimeException(LogCodes.WPH1023E, logmsg, e);
    }
  }

  /**
   * Retrieves data from the given record and loads it into the given resource manager.
   *
   * @param manager the resource manager
   * @param record the data from an input record to be loaded as resources into the manager
   * 
   * @throws RuntimeException if any of the data in the record is invalid for its target purpose.
   */
  protected static void loadRecord(PhoneAreaCodesManager manager, String... record) {
    String country = record[0];
    String areaCode = record[1];

    PhoneAreaCodeResource resource = new PhoneAreaCodeResource(areaCode);

    if (country == null || country.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(country), "country code"));
    }

    // not currently used from the "all-resources" list, but add for completeness
    manager.add(resource);
    manager.add(country, resource);
  }
}
