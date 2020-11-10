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
import com.ibm.whc.deid.models.Continent;
import com.ibm.whc.deid.models.Location;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

public class ContinentManager extends ResourceBasedManager<Continent> {
  /** */
  private static final long serialVersionUID = -610638379564157663L;

  public ContinentManager(String tenantId) {
    super(tenantId, Resource.CONTINENT);

    distanceCalc = new LatLonDistance<Continent>(getItemList());
  }

  protected Map<String, List<Location>> continentListMap;
  protected Map<String, LatLonDistance> latLonTree = null;

  private LatLonDistance<Continent> distanceCalc;
  protected final SecureRandom random = new SecureRandom();

  public Collection<Continent> getItemList() {
    return getValues();
  }

  @Override
  public Collection<ResourceEntry> getResources() {
    return LocalizationManager.getInstance().getResources(Resource.CONTINENT);
  }

  protected void addToContinentList(Continent continent, String countryCode) {
    List<Location> list = continentListMap.get(countryCode);

    if (list == null) {
      list = new ArrayList<>();
      list.add(continent);
      continentListMap.put(countryCode, list);
    } else {
      list.add(continent);
    }
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
  public void init() {
    continentListMap = new HashMap<>();
  }

  /**
   * Gets closest continent.
   *
   * @param identifier the identifier
   * @param k the k
   * @return the closest continent
   */
  public String getClosestContinent(String identifier, int k) {
    Continent continent = this.getKey(identifier);
    if (continent == null) {
      return getRandomKey();
    }

    List<Continent> neighbors = distanceCalc.findNearestK(continent, k);
    if (neighbors == null) {
      return getRandomKey();
    }

    if (k > neighbors.size()) {
      k = neighbors.size();
    }

    return neighbors.get(random.nextInt(k - 1) + 1).getName();
  }
}
