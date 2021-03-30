/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.List;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.Continent;
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * Class that provides access to information about the continents known by the De-Identification
 * service.
 * 
 * <p>
 * Instances of this class are thread-safe.
 */
public class ContinentManager extends LocalizedResourceManager<Continent> {

  private static final LogManager logger = LogManager.getInstance();
  
  protected ContinentManager() {
    // nothing required here
  }
  
  /**
   * Creates a new ContinentManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a ContinentManager instance
   * 
   * @see LocalizationManager
   */
  public static ContinentManager buildContinentManager(String localizationProperty) {
    ContinentManager continentManager = new ContinentManager();

    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty).getResources(Resource.CONTINENT);
    for (ResourceEntry entry : resourceEntries) {

      try (InputStream inputStream = entry.createStream()) {
        String countryCode = entry.getCountryCode();

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {
            String name = line.get(0);
            Double latitude = FileUtils.parseDouble(line.get(1));
            Double longitude = FileUtils.parseDouble(line.get(2));
            Continent continent = new Continent(name, countryCode, latitude, longitude);
            continentManager.add(continent);
            continentManager.add(countryCode, continent);
          }
        }
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return continentManager;
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
    LatLonDistance<Continent> distanceCalc = new LatLonDistance<>(getValues(continent.getNameCountryCode()));
    List<Continent> neighbors = distanceCalc.findNearestK(continent, k);
    if (neighbors == null || neighbors.isEmpty()) {
      return getRandomValue();
    }
    return neighbors.get(random.nextInt(neighbors.size()));
  }
}
