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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class MSISDNManager implements Serializable {
  /** */
  private static final long serialVersionUID = -7892640285929594432L;
  protected static final Collection<ResourceEntry> callingCodesList =
      LocalizationManager.getInstance().getResources(Resource.PHONE_CALLING_CODES);
  protected static final Collection<ResourceEntry> areaCodeResourceList =
      LocalizationManager.getInstance().getResources(Resource.PHONE_AREA_CODES);
  protected static final Collection<ResourceEntry> phoneNumberDigitsList =
      LocalizationManager.getInstance().getResources(Resource.PHONE_NUM_DIGITS);

  protected MapWithRandomPick<String, String> countryCodeMap;
  protected Map<String, Set<String>> areaCodeMapByCountry;
  protected Map<String, List<Integer>> phoneNumberDigitsMap;
  
  private static final SecureRandom random = new SecureRandom();

  private static LogManager logger = LogManager.getInstance();

  protected final String tenantId;

  /**
   * Instantiates a new Msisdn manager.
   *
   * @param tenantId
   */
  public MSISDNManager(String tenantId) {
    this.tenantId = tenantId;
    this.countryCodeMap = new MapWithRandomPick<>(new HashMap<String, String>());
    this.areaCodeMapByCountry = new HashMap<>();
    this.phoneNumberDigitsMap = new HashMap<>();

    readResources(Resource.PHONE_CALLING_CODES, tenantId);
    readResources(Resource.PHONE_AREA_CODES, tenantId);
    readResources(Resource.PHONE_NUM_DIGITS, tenantId);
  }

  protected void readResources(Resource resourceType, String tenantId) {
    switch (resourceType) {
      case PHONE_CALLING_CODES:
        this.countryCodeMap.getMap().putAll(readCountryCodeListFromFile(callingCodesList));
        this.countryCodeMap.setKeyList();
        break;
      case PHONE_AREA_CODES:
        this.areaCodeMapByCountry.putAll(readAreaCodeListFromFile(areaCodeResourceList));
        break;
      case PHONE_NUM_DIGITS:
        this.phoneNumberDigitsMap.putAll(readPhoneNumberDigitListFromFile(phoneNumberDigitsList));
        break;
      default:
        // do nothing
    }
  }

  protected Map<? extends String, ? extends Set<String>> readAreaCodeListFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Set<String>> codes = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String country = line.get(0);
          String areaCode = line.get(1);

          if (!codes.containsKey(country)) {
            codes.put(country, new HashSet<String>());
          }

          codes.get(country).add(areaCode);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH2003E,
            "Failed to load area code list " + " for tenant " + tenantId, e);
      }
    }

    return codes;
  }

  public boolean isValidCountryNumDigits(String countryCode, int inputNumDigits) {
    List<Integer> numDigitsList = phoneNumberDigitsMap.get(countryCode);
    if (numDigitsList == null) {
      // since we do not know the number of phone digits for this country
      // code, assume the digits is valid.
      return true;
    }
    for (Integer numDigits : numDigitsList) {
      if (numDigits.equals(inputNumDigits)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Is valid us number boolean.
   *
   * @param data the data
   * @return the boolean
   */
  public boolean isValidUSNumber(String data) {

    Set<String> areaCodeMap = areaCodeMapByCountry.get("USA");
    if (areaCodeMap == null) {
      return false;
    }

    if (data.length() == 10) {
      for (int i = 0; i < data.length(); i++) {
        char c = data.charAt(i);
        if (c < '0' || c > '9') {
          return false;
        }
      }

      String areaCode = data.substring(0, 3);
      if (areaCodeMap.contains(areaCode)) {
        return true;
      }
    }

    return false;
  }

  protected Map<? extends String, ? extends String> readCountryCodeListFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, String> names = new HashMap<>();

    for (ResourceEntry entry : entries) {
      try (CSVParser reader = Readers.createCSVReaderFromStream(entry.createStream())) {
        for (CSVRecord line : reader) {
          names.put(line.get(0), line.get(1));
        }
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH2003E,
            "Failed to load country code list " + " for tenant " + tenantId, e);
      }
    }

    return names;
  }

  protected Map<? extends String, ? extends List<Integer>> readPhoneNumberDigitListFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, List<Integer>> phoneNumberDigitMap = new HashMap<>();

    for (ResourceEntry entry : entries) {
      try (CSVParser reader = Readers.createCSVReaderFromStream(entry.createStream())) {
        for (CSVRecord line : reader) {
          String currArrayString = line.get(1);
          List<Integer> currList = new ArrayList<>();
          if (currArrayString.contains("-")) {
            String[] currArray = currArrayString.split("-");
            int startNum = Integer.parseInt(currArray[0]);
            int endNum = Integer.parseInt(currArray[1]);
            for (int idx = startNum; idx <= endNum; idx++) {
              currList.add(idx);
            }
          } else if (currArrayString.contains("|")) {
            String[] currArray = currArrayString.split("\\|");
            for (int idx = 0; idx < currArray.length; idx++) {
              currList.add(Integer.parseInt(currArray[idx]));
            }
          } else {
            currList.add(Integer.parseInt(currArrayString));
          }
          phoneNumberDigitMap.put(line.get(0), currList);
        }
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH2003E,
            "Failed to load phone number digit list " + " for tenant " + tenantId, e);
      }
    }

    return phoneNumberDigitMap;
  }

  public int getRandomPhoneNumberDigitsByCountry(String countryCode) {
    List<Integer> numberList = phoneNumberDigitsMap.get(countryCode);

    int randNumber = random.nextInt(numberList.size());
    return numberList.get(randNumber);
  }

  public boolean isCountryWithValidDigitMap(String countryCode) {
    List<Integer> numberList = phoneNumberDigitsMap.get(countryCode);
    if (numberList == null || numberList.isEmpty()) {
      return false;
    } else {
      return true;
    }
  }

  /**
   * Gets random country code.
   *
   * @return the random country code
   */
  public String getRandomCountryCode() {
    return this.countryCodeMap.getRandomKey();
  }

  /**
   * Is valid country code boolean.
   *
   * @param country the country
   * @return the boolean
   */
  public boolean isValidCountryCode(String country) {
    if (country == null || country.length() > 3) {
      return false;
    }

    return countryCodeMap.getMap().containsKey(country.toUpperCase());
  }

}
