/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.Country;
import com.ibm.whc.deid.models.Location;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/** The type Country manager. */
public class CountryManager implements Manager {

  private static final LogManager logger = LogManager.getInstance();

  protected static final String allCountriesName = "__all__";

  protected final Resources resourceType = Resource.COUNTRY;

  protected final SecureRandom random;
  protected final Map<String, MapWithRandomPick<String, Country>[]> countryMap;
  protected final Map<String, List<Location>> countryListMap;

  private LatLonDistance<Country> distanceCalc;

  /**
   * Instantiates a new Country manager.
   * 
   * @paramlocalizationProperty location of the localization property file
   */
  protected CountryManager() {
    this.random = new SecureRandom();
    this.countryMap = new HashMap<>();
    this.countryListMap = new HashMap<>();
  }

  /**
   * Creates a new CountryManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a CountryManager instance
   * 
   * @see LocalizationManager
   */
  public static CountryManager buildCountryManager(String localizationProperty) {
    CountryManager manager = new CountryManager();

    String allCountriesNameToken = manager.getAllCountriesName();

    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty).getResources(manager.resourceType);
    for (ResourceEntry entry : resourceEntries) {

      try (InputStream inputStream = entry.createStream()) {
        String locale = entry.getCountryCode();

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord record : reader) {
            String countryName = record.get(0).toUpperCase();
            String iso2Letter = record.get(1).toUpperCase();
            String iso3Letter = record.get(2).toUpperCase();
            String friendlyName = record.get(3).toUpperCase();
            String continent = record.get(4);
            Double latitude = FileUtils.parseDouble(record.get(5));
            Double longitude = FileUtils.parseDouble(record.get(6));

            /* TODO: temp fix until data is finished */
            if (continent.equals("Unknown")) {
              continue;
            }

            Country country = new Country(countryName, iso2Letter, iso3Letter, continent, latitude,
                longitude, locale);

            manager.addToCountryListMap(country, locale);
            manager.addToCountryListMap(country, allCountriesNameToken);

            manager.addToCountryMap(country, countryName, CountryNameSpecification.NAME, locale);
            manager.addToCountryMap(country, iso2Letter, CountryNameSpecification.ISO2, locale);
            manager.addToCountryMap(country, iso3Letter, CountryNameSpecification.ISO3, locale);

            manager.addToCountryMap(country, countryName, CountryNameSpecification.NAME,
                allCountriesNameToken);
            manager.addToCountryMap(country, iso2Letter, CountryNameSpecification.ISO2,
                allCountriesNameToken);
            manager.addToCountryMap(country, iso3Letter, CountryNameSpecification.ISO3,
                allCountriesNameToken);

            if (!friendlyName.equals("")) {
              manager.addToCountryMap(country, friendlyName, CountryNameSpecification.NAME, locale);
              manager.addToCountryMap(country, friendlyName, CountryNameSpecification.NAME,
                  allCountriesNameToken);
            }
          }
        }
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    manager.postRead();

    return manager;
  }

  /**
   * Gets all countries name.
   *
   * @return the all countries name
   */
  protected final String getAllCountriesName() {
    return CountryManager.allCountriesName;
  }

  private MapWithRandomPick<String, Country>[] initCountryMap() {
    int specSize = CountryNameSpecification.values().length;
    @SuppressWarnings("unchecked")
    MapWithRandomPick<String, Country>[] countryMaps = new MapWithRandomPick[specSize];

    for (int i = 0; i < countryMaps.length; i++) {
      countryMaps[i] = new MapWithRandomPick<>(new HashMap<String, Country>());
    }

    return countryMaps;
  }

  protected void addToCountryListMap(Country country, String countryCode) {
    List<Location> countryList = countryListMap.get(countryCode);

    if (countryList == null) {
      countryList = new ArrayList<>();
      countryList.add(country);
      countryListMap.put(countryCode, countryList);
    } else {
      countryList.add(country);
    }
  }

  protected void addToCountryMap(Country country, String key, CountryNameSpecification spec,
      String locale) {
    MapWithRandomPick<String, Country>[] maps = countryMap.get(locale);

    if (maps == null) {
      maps = initCountryMap();
      maps[spec.ordinal()].getMap().put(key, country);
      countryMap.put(locale, maps);
    } else {
      maps[spec.ordinal()].getMap().put(key, country);
    }
  }

  protected void postRead() {
    for (String key : countryMap.keySet()) {
      MapWithRandomPick<String, Country>[] cntMap = countryMap.get(key);
      for (MapWithRandomPick<?, ?> map : cntMap) {
        map.setKeyList();
      }
    }

    this.distanceCalc = new LatLonDistance<>(getCountries());
  }

  private String getPseudorandomElement(List<Location> keys, String key) {
    long hash = Math.abs(HashUtils.longFromHash(key));

    if (keys == null || keys.size() == 0) {
      return Long.toString(hash);
    }

    int position = (int) (hash % keys.size());
    return ((Country) (keys.get(position))).getName();
  }

  public String getPseudorandom(String identifier) {
    String key = identifier.toUpperCase();
    Country country = lookupCountry(identifier);

    if (country == null) {
      return getPseudorandomElement(this.countryListMap.get(allCountriesName), key);
    } else {
      String countryCode = country.getNameCountryCode();
      return getPseudorandomElement(this.countryListMap.get(countryCode), key);
    }
  }

  private CountryNameSpecification getSpecificationFromName(String name) {
    String key = name.toUpperCase();
    for (CountryNameSpecification e : CountryNameSpecification.values()) {
      if (this.countryMap.get(getAllCountriesName())[e.ordinal()].getMap().containsKey(key)) {
        return e;
      }
    }

    return null;
  }

  @Override
  public String getRandomKey() {
    return getRandomKey(CountryNameSpecification.NAME);
  }

  /**
   * Gets random key.
   *
   * @param spec the spec
   * @return the random key
   */
  public String getRandomKey(CountryNameSpecification spec) {
    MapWithRandomPick<String, Country> map = getSpecificationMap(spec);
    return map.getRandomKey();
  }

  /**
   * Gets random key.
   *
   * @param spec the spec
   * @param locale the locale
   * @return the random key
   */
  public String getRandomKey(CountryNameSpecification spec, String locale) {
    MapWithRandomPick<String, Country> map = countryMap.get(locale)[spec.ordinal()];
    return map.getRandomKey();
  }

  /**
   * Gets random key.
   *
   * @param identifier the exception country
   * @param locale the locale
   * @return the random key
   */
  public String getRandomKey(String identifier, String locale) {
    CountryNameSpecification spec = getSpecificationFromName(identifier);
    if (spec == null) {
      spec = CountryNameSpecification.NAME;
    }

    return getRandomKey(spec, locale);
  }

  /**
   * Gets closest country.
   *
   * @param originalCountry the original country
   * @param k the k
   * @return the closest country
   */
  public String getClosestCountry(String originalCountry, int k) {


    CountryNameSpecification spec = getSpecificationFromName(originalCountry);
    if (spec == null) {
      return getRandomKey(CountryNameSpecification.NAME);
    }

    MapWithRandomPick<String, Country> lookupMap = getSpecificationMap(spec);
    if (lookupMap == null) {
      return getRandomKey(spec);
    }

    String key = originalCountry.toUpperCase();
    Country lookupResult = lookupMap.getMap().get(key);

    List<Country> nearest = distanceCalc.findNearestK(lookupResult, k);
    if (nearest == null) {
      return getRandomKey(spec);
    }
    return (nearest.get(random.nextInt(nearest.size())).getName());
  }

  private MapWithRandomPick<String, Country> getSpecificationMap(CountryNameSpecification spec) {
    return countryMap.get(getAllCountriesName())[spec.ordinal()];
  }

  /**
   * Is valid country boolean.
   *
   * @param countryName the country name
   * @param domain the domain
   * @return the boolean
   */
  public boolean isValidCountry(String countryName, CountryNameSpecification domain) {
    String key = countryName.toUpperCase();
    MapWithRandomPick<?, ?> map = null;

    map = getSpecificationMap(domain);
    if (map == null) {
      return false;
    }

    return map.getMap().containsKey(key);
  }

  /**
   * Lookup country country.
   *
   * @param country the country
   * @param locale the locale
   * @return the country
   */
  public Country lookupCountry(String country, String locale) {
    String key = country.toUpperCase();

    if (locale == null) {
      locale = getAllCountriesName();
    }

    MapWithRandomPick<String, Country>[] maps = countryMap.get(locale);
    if (maps == null) {
      return null;
    }

    for (CountryNameSpecification e : CountryNameSpecification.values()) {
      Country res = maps[e.ordinal()].getMap().get(key);
      if (res != null) {
        return res;
      }
    }

    return null;
  }

  /**
   * Lookup country country.
   *
   * @param country the country
   * @return the country
   */
  public Country lookupCountry(String country) {
    return lookupCountry(country, null);
  }

  @Override
  public boolean isValidKey(String country) {
    String key = country.toUpperCase();
    for (CountryNameSpecification e : CountryNameSpecification.values()) {
      if (countryMap.get(getAllCountriesName())[e.ordinal()].getMap().containsKey(key)) {
        return true;
      }
    }

    return false;
  }

  public Collection<Country> getCountries() {
    List<Location> locations = countryListMap.get(getAllCountriesName());
    int count = locations == null ? 0 : locations.size();
    List<Country> list = new ArrayList<>(count);
    if (count > 0) {
      for (Location location : locations) {
        list.add((Country) location);
      }
    }
    return list;
  }
}
