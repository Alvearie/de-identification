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
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import com.ibm.whc.deid.models.Continent;
import com.ibm.whc.deid.models.Location;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class ContinentManager extends ResourceBasedManager<Continent> {

  private static final long serialVersionUID = -610638379564157663L;

  protected Map<String, List<Location>> continentListMap;

  private transient volatile ConcurrentHashMap<String, LatLonDistance<Continent>> distanceCalcMap =
      null;

  protected final SecureRandom random = new SecureRandom();

  public ContinentManager(String tenantId, String localizationProperty) {
    super(tenantId, Resource.CONTINENT, localizationProperty);
  }

  @Override
  public void init() {
    continentListMap = new HashMap<>();
  }

  @Override
  public Collection<ResourceEntry> getResources() {
		return LocalizationManager.getInstance(localizationProperty).getResources(Resource.CONTINENT);
  }

  @Override
  public Map<String, Map<String, Continent>> readResourcesFromFile(
      Collection<ResourceEntry> entries) {
    Map<String, Map<String, Continent>> continentsPerLocale = new HashMap<>();

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();
      String entryCountryCode = entry.getCountryCode();

      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String name = line.get(0);
          Double latitude = FileUtils.parseDouble(line.get(1));
          Double longitude = FileUtils.parseDouble(line.get(2));
          Continent continent = new Continent(name, entryCountryCode, latitude, longitude);

          addToContinentList(continent, entryCountryCode);

          addToMapByLocale(continentsPerLocale, entryCountryCode, name.toUpperCase(), continent);
          addToMapByLocale(continentsPerLocale, getAllCountriesName(), name.toUpperCase(),
              continent);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return continentsPerLocale;
  }

  @Override
  public Collection<Continent> getItemList() {
    return getValues();
  }

  protected void addToContinentList(Continent continent, String countryCode) {
    List<Location> list = continentListMap.get(countryCode);
    if (list == null) {
      list = new ArrayList<>();
      continentListMap.put(countryCode, list);
    }
    list.add(continent);
  }

  /**
   * Gets closest continent.
   *
   * @param the non-null source continent
   * 
   * @param k the number of nearby continents from which to select
   * 
   * @return a randomly selected continent from the nearby list
   */
  public Continent getClosestContinent(Continent continent, int k) {
    LatLonDistance<Continent> distanceCalc = getDistanceCalculator(continent.getNameCountryCode());
    List<Continent> neighbors = distanceCalc.findNearestK(continent, k);
    if (neighbors == null || neighbors.isEmpty()) {
      return getRandomValue();
    }
    return neighbors.get(random.nextInt(neighbors.size()));
  }

  private LatLonDistance<Continent> getDistanceCalculator(String countryCode) {
    // OK if in a race condition multiple threads re-create this
    ConcurrentHashMap<String, LatLonDistance<Continent>> map = distanceCalcMap;
    if (map == null) {
      map = new ConcurrentHashMap<>();
      distanceCalcMap = map;
    }
    LatLonDistance<Continent> dist = map.get(countryCode);
    if (dist == null) {
      dist = new LatLonDistance<>(getValues(countryCode));
      map.put(countryCode, dist);
    }
    return dist;
  }
}
