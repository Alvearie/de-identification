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
import com.ibm.whc.deid.models.County;
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import com.ibm.whc.deid.utils.log.Messages;

public class CountyManager extends LocalizedResourceManager<County> {

  private static LogManager logger = LogManager.getInstance();

  protected static final Resources resourceType = Resource.COUNTY;

  protected CountyManager() {
    super(3200, 3200);
  }

  /**
   * Creates a new CountyManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a CountyManager instance
   * 
   * @see LocalizationManager
   */
  public static CountyManager buildCountyManager(String localizationProperty) {
    CountyManager manager = new CountyManager();

    try {
      Collection<ResourceEntry> resourceEntries =
          LocalizationManager.getInstance(localizationProperty).getResources(resourceType);
      for (ResourceEntry entry : resourceEntries) {
  
        try (InputStream inputStream = entry.createStream()) {
          String countryCode = entry.getCountryCode();
          String fileName = entry.getFilename();
  
          try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
            for (CSVRecord line : reader) {
              loadCSVRecord(fileName, countryCode, manager, line);
            }
          }
        }
      }
    } catch (IOException e) {
      logger.logError(LogCodes.WPH1013E, e);
      throw new RuntimeException(e);
    }

    return manager;
  }
  
  /**
   * Retrieves data from the given Comma-Separated Values (CSV) record and loads it into the given
   * resource manager.
   *
   * @param fileName the name of the file from which the CSV data was obtained - used for logging
   *        and error messages
   * @param locale the locale or country code to associate with the resource
   * @param manager the resource manager
   * @param record a single record read from a source that provides CSV format data
   * 
   * @throws RuntimeException if any of the data in the record is invalid for its target purpose.
   */
  protected static void loadCSVRecord(String fileName, String locale, CountyManager manager,
      CSVRecord record) {
    try {
      loadRecord(locale, manager, record.get(0), record.get(1), record.get(2), record.get(3));

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
   * @param countryCode the locale or country code to associate with the resource
   * @param manager the resource manager
   * @param record the data from an input record to be loaded as resources into the manager
   * 
   * @throws RuntimeException if any of the data in the record is invalid for its target purpose.
   */
  protected static void loadRecord(String countryCode, CountyManager manager, String... record) {
    String name = record[0];
    String shortName = record[1];
    String state = record[2];
    String population = record[3];

    County county = new County(name, countryCode, shortName, state, population, true);
    manager.add(county);
    manager.add(countryCode, county);

    County countyShort = new County(name, countryCode, shortName, state, population, false);
    manager.add(countyShort);
    manager.add(countryCode, countyShort);
  }
}
