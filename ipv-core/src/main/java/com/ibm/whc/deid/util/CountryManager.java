/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.Serializable;
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
public class CountryManager extends AbstractManager<Country>
    implements Serializable {
  /** */
  private static final long serialVersionUID = -1974159797966269524L;
  protected final Resources resourceType = Resource.COUNTRY;
  protected final Collection<ResourceEntry> resourceCountryList =
      LocalizationManager.getInstance(LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES).getResources(resourceType);
  protected final SecureRandom random;

  protected final Map<String, MapWithRandomPick<String, Country>[]> countryMap;
  protected Map<String, List<Location>> countryListMap;
  protected String tenantId;

  protected static final String allCountriesName = "__all__";
  private LatLonDistance<Country> distanceCalc;


  private static final LogManager logger = LogManager.getInstance();

  /**
   * Gets all countries name.
   *
   * @return the all countries name
   */
  protected final String getAllCountriesName() {
    return CountryManager.allCountriesName;
  }

  /** Instantiates a new Country manager. */
  public CountryManager(String tenantId) {
    this.tenantId = tenantId;
    this.random = new SecureRandom();

    this.countryMap = new HashMap<>();
    this.countryListMap = new HashMap<>();

    readResources(resourceType, tenantId);

    for (String key : countryMap.keySet()) {
      MapWithRandomPick<String, Country>[] cntMap = countryMap.get(key);
      for (MapWithRandomPick<?, ?> map : cntMap) {
        map.setKeyList();
      }
    }

    this.distanceCalc = new LatLonDistance(getItemList());
  }

  protected void readResources(Resources resourceType, String tenantId) {
    readCountryListFromFile(resourceCountryList);
  }

  private MapWithRandomPick<String, Country>[] initCountryMap() {
    int specSize = CountryNameSpecification.values().length;
    @SuppressWarnings("unchecked")
    MapWithRandomPick<String, Country>[] countryMap = new MapWithRandomPick[specSize];

    for (int i = 0; i < countryMap.length; i++) {
      countryMap[i] = new MapWithRandomPick<>(new HashMap<String, Country>());
    }

    return countryMap;
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

  private String getPseudorandomElement(List<Location> keys, String key) {
    Long hash = Math.abs(HashUtils.longFromHash(key, "SHA-256"));

    if (keys == null || keys.size() == 0) {
      return hash.toString();
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

  protected void readCountryListFromFile(Collection<ResourceEntry> entries) {
    for (ResourceEntry entry : entries) {
      String locale = entry.getCountryCode();

      try (CSVParser reader = Readers.createCSVReaderFromStream(entry.createStream())) {
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

          addToCountryListMap(country, locale);
          addToCountryListMap(country, getAllCountriesName());

          addToCountryMap(country, countryName, CountryNameSpecification.NAME, locale);
          addToCountryMap(country, iso2Letter, CountryNameSpecification.ISO2, locale);
          addToCountryMap(country, iso3Letter, CountryNameSpecification.ISO3, locale);

          addToCountryMap(country, countryName, CountryNameSpecification.NAME,
              getAllCountriesName());
          addToCountryMap(country, iso2Letter, CountryNameSpecification.ISO2,
              getAllCountriesName());
          addToCountryMap(country, iso3Letter, CountryNameSpecification.ISO3,
              getAllCountriesName());

          if (!friendlyName.equals("")) {
            addToCountryMap(country, friendlyName, CountryNameSpecification.NAME, locale);
            addToCountryMap(country, friendlyName, CountryNameSpecification.NAME,
                getAllCountriesName());
          }
        }
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
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

  @Override
  public Collection<Country> getItemList() {
    List<Country> list = new ArrayList<>();

    for (Location location : countryListMap.get(getAllCountriesName())) {
      list.add((Country) location);
    }

    return list;
  }

}
