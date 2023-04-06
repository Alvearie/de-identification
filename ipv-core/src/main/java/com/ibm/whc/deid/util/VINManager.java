/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.WorldManufacturerId;
import com.ibm.whc.deid.resources.ResourceManager;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import com.ibm.whc.deid.utils.log.Messages;

public class VINManager extends ResourceManager<WorldManufacturerId> {

  private static LogManager logger = LogManager.getInstance();

  protected final char[] excludedCharacters = {'I', 'O', 'Q', 'i', 'o', 'q'};

  protected static final Resources resourceType = Resource.WORLD_MANUFACTURERS_IDENTIFIER;

  protected VINManager() {
    super(500);
  }

  /**
   * Creates a new VINManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a VINManager instance
   * 
   * @see LocalizationManager
   */
  public static VINManager buildVINManager(String localizationProperty) {
    VINManager manager = new VINManager();

    try {
      Collection<ResourceEntry> resourceEntries =
          LocalizationManager.getInstance(localizationProperty).getResources(resourceType);
      for (ResourceEntry entry : resourceEntries) {

        try (InputStream inputStream = entry.createStream()) {
          String fileName = entry.getFilename();

          try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
            for (CSVRecord line : reader) {
              loadCSVRecord(fileName, manager, line);
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
   * @param manager the resource manager
   * @param record a single record read from a source that provides CSV format data
   * 
   * @throws RuntimeException if any of the data in the record is invalid for its target purpose.
   */
  protected static void loadCSVRecord(String fileName, VINManager manager, CSVRecord record) {
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
  protected static void loadRecord(VINManager manager, String... record) {
    String wmi = record[0];
    String mnf = record[1];
    WorldManufacturerId id = new WorldManufacturerId(wmi, mnf);
    manager.add(id);
  }

  /**
   * Determine whether the input is a valid Vehicle Identification Number (VIN).
   * 
   * @param data the input to examine
   * 
   * @return <i>True</i> if the given value is a valid VIN based on information loaded into this
   *         manager and <i>False</i> if not.
   */
  @Override
  public boolean isValidKey(String data) {

    if (data == null) {
      return false;
    }

    /*
     * All standards for VIN are 17-digit format First 3 digits is WMI - World manufacturer
     * identifier Digits 4-9 are the vehicle description section Digits 10-17 is the vehicle
     * identifier section.
     */
    int dataLength = data.length();
    if (dataLength != 17) {
      return false;
    }

    /*
     * In 1981, the National Highway Traffic Safety Administration of the United States standardized
     * the format.[1] It required all over-the-road-vehicles sold to contain a 17-character VIN,
     * which does not include the letters I (i), O (o), or Q (q) (to avoid confusion with numerals 1
     * and 0).
     */
    // data = data.toUpperCase();
    for (int i = 0; i < dataLength; i++) {
      char c = data.charAt(i);

      /* VIN is composed only of digits and letters */
      if (!Character.isDigit(c) && !Character.isLetter(c)) {
        return false;
      }
      if (c == 'I' || c == 'i' || c == 'O' || c == 'o' || c == 'Q' || c == 'q') {
        return false;
      }
    }

    /* check if the WMI is one of the known */
    String wmi = data.substring(0, 3);
    if (!isValidWMI(wmi)) {
      return false;
    }

    return true;
  }

  /**
   * Determines whether the given value is a known World Manufacturer Identifier (WMI).
   *
   * @param wmi the value to check
   * 
   * @return <i>True</i> if the given value is a known WMI and <i>false</i> otherwise.
   */
  public boolean isValidWMI(String wmi) {
    return super.isValidKey(wmi);
  }

  @Override
  public String getRandomKey() {
    return getRandomWMI()
        + RandomGenerators.randomUIDGenerator(14, excludedCharacters).toUpperCase();
  }

  /**
   * Gets a random World Manufacturer Identifier (WMI).
   *
   * @return a random WMI or <i>null</i> if no WMI information has been loaded.
   */
  public String getRandomWMI() {
    return super.getRandomKey();
  }
}
