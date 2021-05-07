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
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import com.ibm.whc.deid.utils.log.Messages;

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

    try {
      Collection<ResourceEntry> resourceEntries =
          LocalizationManager.getInstance(localizationProperty).getResources(Resource.CONTINENT);
      for (ResourceEntry entry : resourceEntries) {

        try (InputStream inputStream = entry.createStream()) {
          String countryCode = entry.getCountryCode();
          String fileName = entry.getFilename();

          try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
            for (CSVRecord line : reader) {
              loadCSVRecord(fileName, countryCode, continentManager, line);
            }
          }
        }
      }
    } catch (IOException e) {
      logger.logError(LogCodes.WPH1013E, e);
      throw new RuntimeException(e);
    }

    return continentManager;
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
  protected static void loadCSVRecord(String fileName, String locale, ContinentManager manager,
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
  protected static void loadRecord(String locale, ContinentManager manager, String... record) {
    String name = record[0];
    String latitude = record[1];
    String longitude = record[2];
    Continent continent = new Continent(name, locale, latitude, longitude);
    manager.add(continent);
    manager.add(locale, continent);
  }

  /**
   * Returns a random continent among the given number of continents closest to the given continent.
   *
   * @param continent the starting continent
   * @param k the number of closest continent to use as candidates for selection
   * 
   * @return the selected continent or <i>null</i> if no continent can be selected
   */
  public Continent getClosestContinent(Continent continent, int k) {
    Continent selected = null;
    if (continent != null && continent.getLocation() != null) {
      LatLonDistance<Continent> distanceCalc = new LatLonDistance<>(getValues());
      List<Continent> neighbors = distanceCalc.findNearestK(continent, k);
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
