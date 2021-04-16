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
import com.ibm.whc.deid.models.WorldManufacturerId;
import com.ibm.whc.deid.resources.ResourceManager;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class VINManager extends ResourceManager<WorldManufacturerId> {

  private static LogManager logger = LogManager.getInstance();

  protected final char[] excludedCharacters = {'I', 'O', 'Q', 'i', 'o', 'q'};

  protected static final Resources resourceType = Resource.WORLD_MANUFACTURERS_IDENTIFIER;

  protected VINManager() {
    // nothing required here
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

    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty).getResources(resourceType);
    for (ResourceEntry entry : resourceEntries) {

      try (InputStream inputStream = entry.createStream()) {
        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {
            
            String wmi = line.get(0);
            String mnf = line.get(1);
            
            if (!wmi.trim().isEmpty() && !mnf.trim().isEmpty()) {
              if (wmi.length() != 3) {
                logger.logWarn(LogCodes.WPH1014W, wmi, "vin.wmi");
              } else {
                WorldManufacturerId id = new WorldManufacturerId(wmi, mnf);
                manager.add(id);
              }
            }
          }
        }

      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return manager;
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
