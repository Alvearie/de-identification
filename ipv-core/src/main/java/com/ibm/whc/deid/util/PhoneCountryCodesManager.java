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
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import com.ibm.whc.deid.utils.log.Messages;

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
    super(300);
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

    try {
      Collection<ResourceEntry> resourceEntries = LocalizationManager
          .getInstance(localizationProperty).getResources(Resource.PHONE_CALLING_CODES);

      for (ResourceEntry entry : resourceEntries) {
        try (InputStream inputStream = entry.createStream()) {

          try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
            for (CSVRecord line : reader) {
              try {
                loadRecord(mgr, line.get(0), line.get(1));

              } catch (RuntimeException e) {
                // CSVRecord has a very descriptive toString() implementation
                String logmsg = Messages.getMessage(LogCodes.WPH1023E, String.valueOf(line),
                    entry.getFilename(), e.getMessage());
                throw new KeyedRuntimeException(LogCodes.WPH1023E, logmsg, e);
              }
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
   * Retrieves data from the given record and loads it into the given resource manager.
   *
   * @param locale the locale or country code to associate with the resource
   * @param manager the resource manager
   * @param record the data from an input record to be loaded as resources into the manager
   * 
   * @throws RuntimeException if any of the data in the record is invalid for its target purpose.
   */
  protected static void loadRecord(PhoneCountryCodesManager manager, String... record) {
    String key = record[0];
    String value = record[1];
    manager.add(new KeyValueResource(key, value));
  }
}
