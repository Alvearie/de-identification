/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.ZIPCode;
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import com.ibm.whc.deid.utils.log.Messages;

public class ZIPCodeManager extends LocalizedResourceManager<ZIPCode> {

  private static LogManager logger = LogManager.getInstance();

  protected static final Resources resourceType = Resource.ZIPCODE;

  /**
   * Maps country codes (key) to the length of a zip code (value) in that country/locale.
   */
  private Map<String, Integer> lengthsMap = new HashMap<>();

  /**
   * Maps country codes (key) to the replacement prefix (value) to use when a zip code does not meet
   * requirements.
   */
  private Map<String, String> replacementsMap = new HashMap<>();

  protected ZIPCodeManager() {
    super(34000, 34000);
  }

  /**
   * Creates a new ZIPCodeManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a ZIPCodeManager instance
   * 
   * @see LocalizationManager
   */
  public static ZIPCodeManager buildZIPCodeManager(String localizationProperty) {
    ZIPCodeManager manager = new ZIPCodeManager();

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

    populateCountryProperties(manager, localizationProperty);

    return manager;
  }


  /**
   * Retrieves data from the given Comma-Separated Values (CSV) record and loads it into the given
   * resource manager.
   *
   * @param fileName the name of the file from which the CSV data was obtained - used for logging
   *        and error messages
   * @param countryCode the locale or country code to associate with the resource
   * @param manager the resource manager
   * @param record a single record read from a source that provides CSV format data
   * 
   * @throws RuntimeException if any of the data in the record is invalid for its target purpose.
   */
  protected static void loadCSVRecord(String fileName, String countryCode, ZIPCodeManager manager,
      CSVRecord record) {
    try {
      loadRecord(countryCode, manager, record.get(0), record.get(1));

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
  protected static void loadRecord(String countryCode, ZIPCodeManager manager, String... record) {
    String code = record[0];
    String populationString = record[1];
    ZIPCode zipCode = new ZIPCode(code, populationString);
    manager.add(zipCode);
    manager.add(countryCode, zipCode);
  }

  protected static void populateCountryProperties(ZIPCodeManager manager,
      String localizationProperty) {
    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty).getResources(resourceType);

    for (ResourceEntry entry : resourceEntries) {
      String countryCode = entry.getCountryCode();
      if (countryCode != null && !countryCode.trim().isEmpty()) {
        countryCode = countryCode.toLowerCase();

        Properties localeProp =
            LocalizationManager.getInstance(localizationProperty).getLocaleProperties(countryCode);
        String zipcodeLengthString = localeProp.getProperty("zipcode.length");
        if (zipcodeLengthString != null) {
          Integer length = null;
          try {
            length = Integer.valueOf(zipcodeLengthString);
            manager.lengthsMap.put(countryCode, length);
          } catch (NumberFormatException e) {
            logger.logWarn(LogCodes.WPH1014W, zipcodeLengthString, "zipcode.length");
          }
        }

        String zipcodeReplacement = localeProp.getProperty("zipcode.underPopulated");
        if (zipcodeReplacement != null) {
          manager.replacementsMap.put(countryCode, zipcodeReplacement);
        }
      }
    }
  }

  /**
   * Returns the length of zip codes in the current region
   *
   * @param countryCode The 2 digit ISO country code of the given country
   * @return the total number of digits required to represent a zip code in the given country or
   *         <b>-1</b> if the number of digits is unknown.
   */
  public int getZipCodeLength(String countryCode) {
    Integer length = null;
    if (countryCode != null) {
      length = lengthsMap.get(countryCode.toLowerCase());
    }
    return length == null ? -1 : length.intValue();
  }

  /**
   * Get the country-specific zip code replacement pattern for zips that do not meet the criteria
   * for masking via prefix.
   *
   * @param countryCode The ISO 2 character country code for the target country
   * 
   * @return the replacement zip code prefix or an empty string if no replacement has been loaded
   *         for the given country
   */
  public String getZipCodeReplacement(String countryCode) {
    String replacement = null;
    if (countryCode != null) {
      replacement = replacementsMap.get(countryCode.toLowerCase());
    }
    return replacement == null ? "" : replacement;
  }

  /**
   * Gets the total population of ZIP codes for the given country with the given ZIP code prefix.
   *
   * @param countryCode the country code
   * @param zipCodePrefix the ZIP code prefix
   * 
   * @return the total population of all known ZIP codes that begin with the given prefix or
   *         <i>null</i> if any of the input is <i>null</i>.
   */
  public Integer getPopulationByPrefix(String countryCode, String zipCodePrefix) {
    Integer populationI = null;
    if (countryCode != null && zipCodePrefix != null) {
      int population = 0;
      for (ZIPCode code : getValues(countryCode.toLowerCase())) {
        if (code.getCode().startsWith(zipCodePrefix)) {
          population += code.getPopulation();
        }
      }
      populationI = Integer.valueOf(population);
    }
    return populationI;
  }

  /**
   * Gets a random ZIP code in the given country with the given ZIP code prefix.
   *
   * @param countryCode the country code
   * @param zipCodePrefix the ZIP code prefix
   * 
   * @return a random ZIP code from the given country with the given prefix or <i>null</i> if no
   *         such ZIP codes have been loaded
   */
  public String getRandomZipCodeByPrefix(String countryCode, String zipCodePrefix) {
    String code = null;
    if (countryCode != null && zipCodePrefix != null) {

      ArrayList<ZIPCode> candidates = new ArrayList<>(100);
      for (ZIPCode zcode : getValues(countryCode)) {
        if (zcode.getCode().startsWith(zipCodePrefix)) {
          candidates.add(zcode);
        }
      }

      int count = candidates.size();
      if (count == 1) {
        code = candidates.get(0).getCode();
      } else if (count > 1) {
        code = candidates.get(random.nextInt(count)).getCode();
      }
    }

    return code;
  }
}
