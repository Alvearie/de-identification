/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.SWIFTCode;
import com.ibm.whc.deid.resources.ResourceManager;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import com.ibm.whc.deid.utils.log.Messages;

/**
 * Class that provides access to information about the SWIFT codes known by the De-Identification
 * service.
 * 
 * <p>
 * Instances of this class are thread-safe.
 */
public class SWIFTCodeManager extends ResourceManager<SWIFTCode> {

  private static final LogManager logger = LogManager.getInstance();

  /**
   * SWIFT codes contain a two-character country code. Map each country code represented in the
   * resources to the list of codes for that country.
   * 
   * <p>
   * Note - this Map is only modified during construction of the instance
   */
  protected final Map<String, List<SWIFTCode>> codeByCountryMap = new HashMap<>();

  protected SWIFTCodeManager() {
    // nothing required here
  }

  /**
   * Creates a new SWIFTCodeManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a SWIFTCodeManager instance
   * 
   * @see LocalizationManager
   */
  public static SWIFTCodeManager buildSWIFTCodeManager(String localizationProperty) {
    SWIFTCodeManager swiftCodeManager = new SWIFTCodeManager();

    try {
      Collection<ResourceEntry> resourceEntries =
          LocalizationManager.getInstance(localizationProperty).getResources(Resource.SWIFT);
      for (ResourceEntry entry : resourceEntries) {
        try (InputStream inputStream = entry.createStream()) {
          String fileName = entry.getFilename();

          try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
            for (CSVRecord line : reader) {
              loadCSVRecord(fileName, swiftCodeManager, line);
            }
          }
        }
      }
    } catch (IOException e) {
      logger.logError(LogCodes.WPH1013E, e);
      throw new RuntimeException(e);
    }

    return swiftCodeManager;
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
  protected static void loadCSVRecord(String fileName, SWIFTCodeManager manager, CSVRecord record) {
    try {
      loadRecord(manager, record.get(0));

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
  protected static void loadRecord(SWIFTCodeManager manager, String... record) {
    SWIFTCode swiftCode = new SWIFTCode(record[0]);
    manager.addSWIFTCode(swiftCode);
  }

  protected void addSWIFTCode(SWIFTCode swiftCode) {
    add(swiftCode);

    String ccKey = swiftCode.getCountry();
    List<SWIFTCode> countryList = codeByCountryMap.get(ccKey);
    if (countryList == null) {
      countryList = new ArrayList<>();
      codeByCountryMap.put(ccKey, countryList);
    }
    countryList.add(swiftCode);
  }

  /**
   * Retrieves a random SWIFT code value from the known SWIFT codes from the given country.
   *
   * @param code the SWIFT country code
   * 
   * @return a random code value from the indicated country or <i>null</i> if no such codes are
   *         available
   */
  public String getRandomValueFromCountry(String countryCode) {
    String code = null;
    if (countryCode != null) {
      List<SWIFTCode> list = codeByCountryMap.get(countryCode.toUpperCase());
      if (list != null && !list.isEmpty()) {
        SWIFTCode randomCode = list.get(random.nextInt(list.size()));
        code = randomCode.getCode();
      }
    }
    return code;
  }
}
