/*
 * © Merative US L.P. 2016,2021
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
import com.ibm.whc.deid.models.City;
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import com.ibm.whc.deid.utils.log.Messages;

/**
 * Class that provides access to information about the cities known by the De-Identification
 * service.
 * 
 * <p>
 * Instances of this class are thread-safe.
 */
public class CityManager extends LocalizedResourceManager<City> {

  private static final LogManager logger = LogManager.getInstance();

  protected CityManager() {
    super(3000, 3000);
  }

  /**
   * Creates a new CityManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a CityManager instance
   * 
   * @see LocalizationManager
   */
  public static CityManager buildCityManager(String localizationProperty) {
    CityManager cityManager = new CityManager();

    try {
      Collection<ResourceEntry> resourceEntries =
          LocalizationManager.getInstance(localizationProperty).getResources(Resource.CITY);
      for (ResourceEntry entry : resourceEntries) {

        try (InputStream inputStream = entry.createStream()) {
          String locale = entry.getCountryCode();
          String fileName = entry.getFilename();

          try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
            for (CSVRecord line : reader) {
              loadCSVRecord(fileName, locale, cityManager, line);
            }
          }
        }
      }
    } catch (IOException e) {
      logger.logError(LogCodes.WPH1013E, e);
      throw new RuntimeException(e);
    }

    return cityManager;
  }

  /**
   * Retrieves data from the given Comma-Separated Values (CSV) record and loads it into the given
   * resource manager.
   *
   * @param fileName the name of the file from which the CSV data was obtained - used for logging
   *        and error messages
   * @param locale the locale or country code to associate with the resource
   * @param manager the resource manager
   * @param record a single record read from a source that provides CSV format data
   * 
   * @throws RuntimeException if any of the data in the record is invalid for its target purpose.
   */
  protected static void loadCSVRecord(String fileName, String locale, CityManager manager,
      CSVRecord record) {
    try {
      loadRecord(locale, manager, record.get(0), record.get(1), record.get(2));

    } catch (RuntimeException e) {
      // CSVRecord has a very descriptive toString() implementation
      String logmsg =
          Messages.getMessage(LogCodes.WPH1023E, String.valueOf(record), fileName, e.getMessage());
      throw new KeyedRuntimeException(LogCodes.WPH1023E, logmsg, e);
    }
  }

  /**
   * Retrieves data from the given record and loads it into the given resource manager.
   *
   * @param locale the locale or country code to associate with the resource
   * @param manager the resource manager
   * @param record the data from an input record to be loaded as resources into the manager
   * 
   * @throws RuntimeException if any of the data in the record is invalid for its target purpose.
   */
  protected static void loadRecord(String locale, CityManager manager, String... record) {
    String name = record[0];
    String latitude = record[1];
    String longitude = record[2];
    City city = new City(name, latitude, longitude, locale);
    manager.add(city);
    manager.add(locale, city);
  }

  /**
   * Returns a random city among the given number of cities closest to the given city.
   *
   * @param city the starting city
   * @param k the number of closest cities to use as candidates for selection
   * 
   * @return the selected city or <i>null</i> if no city can be selected
   */
  public City getClosestCity(City city, int k) {
    City selected = null;
    if (city != null && city.getLocation() != null) {
      LatLonDistance<City> distanceCalc = new LatLonDistance<>(getValues());
      List<City> neighbors = distanceCalc.findNearestK(city, k);
      if (neighbors != null && !neighbors.isEmpty()) {
        int count = neighbors.size();
        if (count == 1) {
          selected = neighbors.get(0);
        } else {
          selected = neighbors.get(random.nextInt(count));
        }
      }
    }
    return selected;
  }
}
