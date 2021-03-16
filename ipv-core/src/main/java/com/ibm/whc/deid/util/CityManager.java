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
import com.ibm.whc.deid.models.City;
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class CityManager extends LocalizedResourceManager<City> {

  private static final LogManager logger = LogManager.getInstance();
    
  protected CityManager() {
    // nothing required here
  }
  
  public static CityManager getCityManager(String localizationProperty) {
    CityManager cityManager = new CityManager();
    
    Collection<ResourceEntry> resourceEntries = LocalizationManager.getInstance(localizationProperty).getResources(Resource.CITY);    
    for (ResourceEntry entry : resourceEntries) {
      
      try (InputStream inputStream = entry.createStream()) {
        String locale = entry.getCountryCode();    
        
        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {
            String name = line.get(0);
            Double latitude = FileUtils.parseDouble(line.get(1));
            Double longitude = FileUtils.parseDouble(line.get(2));
            String countryCode = line.get(3);
            City city = new City(name, latitude, longitude, countryCode, locale);
  
            String key = city.getKey();
            cityManager.add(key, city);
            cityManager.add(locale, key, city);
          }
        }
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    return cityManager;
  }

  /**
   * Gets closest city.
   *
   * @param city the city
   * @param k the k
   * @return the closest city
   */
  public String getClosestCity(String cityString, int k) {
    String value = null;
    
    City city = getValue(cityString);
    
    if (city == null) {
      city = getRandomValue();
      if (city != null) {
        value = city.getName();
      }
      
    } else {
      LatLonDistance<City> distanceCalc = new LatLonDistance<>(getValues());
      List<City> neighbors = distanceCalc.findNearestK(city, k);
      if (neighbors != null && !neighbors.isEmpty()) {
        int count = neighbors.size();
        if (count == 1) {
          value = neighbors.get(0).getName();
        } else {
          value = neighbors.get(random.nextInt(count)).getName();
        }
      }
    }
    
    return value;
  }
}
