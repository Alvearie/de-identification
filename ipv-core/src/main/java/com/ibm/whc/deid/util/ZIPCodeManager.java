/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import com.ibm.whc.deid.models.ZIPCode;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class ZIPCodeManager extends ResourceBasedManager<ZIPCode> implements Serializable {
  /** */
  private static final long serialVersionUID = 4163065702734228622L;

  /** */
  // Map of country to <Map of prefix to <<Map of zip code to ZIPCODE>>
  private Map<String, Map<String, MapWithRandomPick<String, ZIPCode>>> countryCodeMap;
  private final int prefixLength;

  /**
   * Instantiates a ZIP code manager.
   *
   * @param prefixLength the prefix length of ZIP codes
 * @param tenantId tenant id
 * @param localizationProperty TODO
   */
  public ZIPCodeManager(int prefixLength, String tenantId, String localizationProperty) {
    super(tenantId, Resource.ZIPCODE, localizationProperty);
    this.prefixLength = prefixLength;
    buildPrefixMap(getResources());
  }

  public int getPrefixLength() {
    return prefixLength;
  }

  @Override
  public void init() {
    countryCodeMap = new HashMap<>();
  }

  @Override
  public Collection<ResourceEntry> getResources() {
		return LocalizationManager.getInstance(localizationProperty).getResources(Resource.ZIPCODE);
  }

  /**
   * Adds ZIP code information to the map.
   *
   * @param zipCode the zip code
   * @param countryCode the country code
   * @param population the population of this ZIP code
   */
  private void addToPrefixMap(String zipCode, String countryCode, Integer population) {
    Map<String, MapWithRandomPick<String, ZIPCode>> prefixMap = countryCodeMap.get(countryCode);
    if (prefixMap == null) {
      prefixMap = new HashMap<>();
      countryCodeMap.put(countryCode, prefixMap);
    }

    String prefix = zipCode.toUpperCase();
    if (prefix.length() > prefixLength) {
      prefix = prefix.substring(0, prefixLength);
    }

    MapWithRandomPick<String, ZIPCode> zipCodeMap = prefixMap.get(prefix);
    if (zipCodeMap == null) {
      zipCodeMap = new MapWithRandomPick<>(new HashMap<String, ZIPCode>());
      prefixMap.put(prefix, zipCodeMap);
    }

    zipCodeMap.getMap().put(zipCode, new ZIPCode(zipCode, population));
    zipCodeMap.setKeyList();
  }

  /**
   * Builds a map of all prefixes and populations given the masking prefix length
   *
   * @param entries The list of resources, each a country from the resources
   */
  private void buildPrefixMap(Collection<ResourceEntry> entries) {
    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      String countryCode = entry.getCountryCode();

      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String code = line.get(0);
          String populationString = line.get(1);
          Integer population = Integer.valueOf(populationString);
          addToPrefixMap(code, countryCode, population);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }
  }

  @Override
  public Map<String, Map<String, ZIPCode>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, ZIPCode>> codes = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      String countryCode = entry.getCountryCode();

      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String code = line.get(0);
          String populationString = line.get(1);

          Integer population = Integer.valueOf(populationString);
          ZIPCode zipCode = new ZIPCode(code, population);

          String key = code.toUpperCase();
          addToMapByLocale(codes, countryCode, key, zipCode);
          addToMapByLocale(codes, getAllCountriesName(), key, zipCode);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return codes;
  }

  /**
   * Gets the population of the specified ZIP code.
   *
   * @param countryCode the country code
   * @param zipCode the ZIP code
   * @return the population
   */
  public Integer getPopulation(String countryCode, String zipCode) {
    ZIPCode zipCodeObj = getKey(countryCode, zipCode);
    if (zipCodeObj == null) {
      return 0;
    }

    return zipCodeObj.getPopulation();
  }

  /**
   * Returns the length of zip codes in the current region
   *
   * @param countryCode The 2 digit ISO country code of the given country
   * @return An Integer value representing the total number of digits required to represent a zip in
   *         the given country.
   */
  public Integer getZipCodeLength(String countryCode) {
    Properties locProp = LocalizationManager.getInstance(LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES).getLocaleProperties(countryCode);
    if (locProp.size() != 0) {
      String zipcodeLengthString = locProp.getProperty("zipcode.length");
      if (null != zipcodeLengthString) {
        return Integer.parseInt(zipcodeLengthString);
      }
    }
    return 0;
  }

  /**
   * Get the country-specific zip code replacement pattern for zips that do not meet the criteria
   * for masking via prefix.
   *
   * @param countryCode The ISO 2 character country code to get the replacement string for
   * @return
   */
  public String getZipCodeReplacement(String countryCode) {
    // Get the localization properties for this country
    Properties locProp = LocalizationManager.getInstance(LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES).getLocaleProperties(countryCode);
    if (locProp.size() != 0) {
      // If the localization properties exist, use the appropriate key
      String zipcodeReplacement = locProp.getProperty("zipcode.underPopulated");
      if (null != zipcodeReplacement) {
        return zipcodeReplacement;
      }
    }
    // Otherwise, return an empty string by default
    return "";
  }

  /**
   * Gets the total population of ZIP codes with same prefix as the specified ZIP code.
   *
   * @param countryCode the country code
   * @param zipCode the ZIP code
   * @return the list of ZIP codes
   */
  public Integer getPopulationByPrefix(String countryCode, String zipCode) {
    if (zipCode.length() < this.prefixLength) {
      return 0;
    }

    Map<String, MapWithRandomPick<String, ZIPCode>> prefixMap =
        countryCodeMap.get(countryCode.toLowerCase());
    if (prefixMap == null) {
      return 0;
    }

    String prefix = zipCode.substring(0, this.prefixLength);
    MapWithRandomPick<String, ZIPCode> zipCodeMap = prefixMap.get(prefix);
    if (zipCodeMap == null) {
      return 0;
    }

    int totalPopulation = 0;
    for (ZIPCode zipCode1 : zipCodeMap.getMap().values()) {
      totalPopulation += zipCode1.getPopulation();
    }

    return totalPopulation;
  }

  /**
   * Gets a random ZIP code with same prefix as the specified ZIP code.
   *
   * @param countryCode the country code
   * @param zipCode the ZIP code
   * @return a random ZIP code with the same prefix, or null
   */
  public String getRandomZipCodeByPrefix(String countryCode, String zipCode) {
    if (zipCode.length() < this.prefixLength) {
      return null;
    }

    Map<String, MapWithRandomPick<String, ZIPCode>> prefixMap =
        countryCodeMap.get(countryCode.toLowerCase());
    if (prefixMap == null) {
      return null;
    }

    String prefix = zipCode.substring(0, this.prefixLength);
    MapWithRandomPick<String, ZIPCode> zipCodeMap = prefixMap.get(prefix);
    if (zipCodeMap == null || zipCodeMap.getMap().isEmpty()) {
      return null;
    }

    return zipCodeMap.getRandomKey();
  }

  @Override
  public Collection<ZIPCode> getItemList() {
    return getValues();
  }
}
