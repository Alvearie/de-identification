/*
 * (C) Copyright IBM Corp. 2016,2020
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

import com.ibm.whc.deid.models.City;
import com.ibm.whc.deid.models.Location;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class CityManager extends ResourceBasedManager<City> {
  /** */
  private static final long serialVersionUID = -3274559886741195053L;

  private final SecureRandom random = new SecureRandom();

  private Map<String, List<Location>> cityListMap;
  private Map<String, LatLonDistance> latLonTree = null;

  private LatLonDistance<City> distanceCalc;

	public CityManager(String tenantId, String localizationProperty) {
		super(tenantId, Resource.CITY, localizationProperty);

    distanceCalc = new LatLonDistance<City>(getItemList());
  }

  @Override
  public Collection<ResourceEntry> getResources() {
		return LocalizationManager.getInstance(localizationProperty).getResources(Resource.CITY);
  }

  protected void addToCityList(City city, String countryCode) {
    List<Location> list = cityListMap.get(countryCode);

    if (list == null) {
      list = new ArrayList<>();
      list.add(city);
      cityListMap.put(countryCode, list);
    } else {
      list.add(city);
    }
  }

  @Override
  public Map<String, Map<String, City>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, City>> cities = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      String locale = entry.getCountryCode();

      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String name = line.get(0);
          Double latitude = FileUtils.parseDouble(line.get(1));
          Double longitude = FileUtils.parseDouble(line.get(2));
          String countryCode = line.get(3);
          City city = new City(name, latitude, longitude, countryCode, locale);

          addToCityList(city, locale);

          addToMapByLocale(cities, locale, name.toUpperCase(), city);
          addToMapByLocale(cities, getAllCountriesName(), name.toUpperCase(), city);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return cities;
  }

  @Override
public void init() {
    this.cityListMap = new HashMap<>();
  }

  @Override
public void postInit() {
    this.latLonTree = new HashMap<>();

    for (String key : cityListMap.keySet()) {
      try {
        this.latLonTree.put(key, new LatLonDistance(cityListMap.get(key)));
      } catch (Exception e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }
  }

  /**
   * Gets closest city.
   *
   * @param city the city
   * @param k the k
   * @return the closest city
   */
  public String getClosestCity(String city, int k) {
    String key = city.toUpperCase();
    City lookup = getKey(key);

    if (lookup == null) {
      return getRandomKey();
    }

    List<City> neighbors = distanceCalc.findNearestK(lookup, k);

    return (neighbors.get(random.nextInt(neighbors.size()))).getName();
  }

  @Override
  public Collection<City> getItemList() {
    return getValues();
  }

}
