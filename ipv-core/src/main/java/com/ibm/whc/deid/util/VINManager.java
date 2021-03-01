/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.security.SecureRandom;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class VINManager implements Manager, Serializable {
  /** */
  private static final long serialVersionUID = 8854083379768714880L;

	protected final Collection<ResourceEntry> resourceWMIList;
  protected final Map<String, String> wmiMap;
  protected final String[] wmiList;
  protected final SecureRandom random;
  protected final char[] excludedCharacters = {'I', 'O', 'Q', 'i', 'o', 'q'};

  private static LogManager logger = LogManager.getInstance();
  protected final Resources resourceType = Resource.WORLD_MANUFACTURERS_IDENTIFIER;

  protected final String tenantId;

  	/**
	 * Instantiates a new Vin manager.
	 *
	 * @param tenantId
	 * @param localizationProperties TODO
	 */
	public VINManager(String tenantId, String localizationProperty) {
    this.tenantId = tenantId;

		resourceWMIList = LocalizationManager.getInstance(localizationProperty)
				.getResources(Resource.WORLD_MANUFACTURERS_IDENTIFIER);
    this.wmiMap = new HashMap<String, String>();

    readResources(resourceType, tenantId);

    this.random = new SecureRandom();

    Set<String> keyset = wmiMap.keySet();
    this.wmiList = keyset.toArray(new String[keyset.size()]);

    for (String wmi : wmiList) {
      if (!isValidWMI(wmi)) {
        throw new RuntimeException("invalid WMI loaded:" + wmi);
      }
    }
  }

  protected void readResources(Resources resourceType, String tenantId) {
    this.wmiMap.putAll(readWMIListFromFile(resourceWMIList));
  }

  protected Map<? extends String, ? extends String> readWMIListFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, String> names = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          names.put(line.get(0).toUpperCase(), line.get(1));
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return names;
  }

  @Override
  public boolean isValidKey(String data) {

    /*
     * All standards for VIN are 17-digit format First 3 digits is WMI - World manufacturer
     * identifier Digits 4-9 are the vehicle description section Digits 10-17 is the vehicle
     * identifier section
     */
    if (data.length() != 17) {
      return false;
    }

    /*
     * In 1981, the National Highway Traffic Safety Administration of the United States standardized
     * the format.[1] It required all over-the-road-vehicles sold to contain a 17-character VIN,
     * which does not include the letters I (i), O (o), or Q (q) (to avoid confusion with numerals 1
     * and 0).
     */
    // data = data.toUpperCase();
    for (int i = 0; i < data.length(); i++) {
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

  @Override
  public String getRandomKey() {
    return getRandomWMI()
        + RandomGenerators.randomUIDGenerator(14, excludedCharacters).toUpperCase();
  }

  /**
   * Gets random wmi.
   *
   * @return the random wmi
   */
  public String getRandomWMI() {
    int index = random.nextInt(wmiList.length);
    return this.wmiList[index];
  }

  /**
   * Gets random wmi.
   *
   * @param exceptionWMI the exception wmi
   * @return the random wmi
   */
  public String getRandomWMI(String exceptionWMI) {
    String randomWmi = getRandomWMI();
    if (randomWmi.equals(exceptionWMI)) {
      return getRandomWMI(exceptionWMI);
    }
    return randomWmi;
  }

  /**
   * Is valid wmi boolean.
   *
   * @param wmi the wmi
   * @return the boolean
   */
  public boolean isValidWMI(String wmi) {
    if (wmi.length() != 3) {
      return false;
    }

    return wmiMap.containsKey(wmi.toUpperCase());
  }

}
