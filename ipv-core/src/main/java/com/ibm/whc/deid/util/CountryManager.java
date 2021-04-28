/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.security.SecureRandom;
import java.util.Collection;
import java.util.List;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.Country;
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import com.ibm.whc.deid.utils.log.Messages;

/**
 * Class that manages known countries loaded into the service.
 */
public class CountryManager implements Manager {

  private static final LogManager logger = LogManager.getInstance();

  private static class CountrySpecificationResourceManager
      extends LocalizedResourceManager<Country> {

    public CountrySpecificationResourceManager(int expectedCount) {
      super(expectedCount);
    }

    protected SecureRandom getRandom() {
      return random;
    }

    @Override
    protected void add(Country resource) {
      super.add(resource);
    }

    @Override
    protected void add(String localeCode, Country resource) {
      super.add(localeCode, resource);
    }
  }

  protected final Resources resourceType = Resource.COUNTRY;

  protected final CountrySpecificationResourceManager countryNames =
      new CountrySpecificationResourceManager(400);
  protected final CountrySpecificationResourceManager countryISO2Codes =
      new CountrySpecificationResourceManager(400);
  protected final CountrySpecificationResourceManager countryISO3Codes =
      new CountrySpecificationResourceManager(400);

  /**
   * Instantiates a new country manager.
   */
  protected CountryManager() {
    // nothing required here
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

    try {
      Collection<ResourceEntry> resourceEntries =
          LocalizationManager.getInstance(localizationProperty).getResources(manager.resourceType);
      for (ResourceEntry entry : resourceEntries) {

        try (InputStream inputStream = entry.createStream()) {
          String locale = entry.getCountryCode();

          try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
            for (CSVRecord record : reader) {
              try {
                String countryName = record.get(0);
                String iso2Letter = record.get(1);
                String iso3Letter = record.get(2);
                String friendlyName = record.get(3);
                String continent = record.get(4);
                /* TODO: temp fix until data is finished */
                if (continent.equals("Unknown")) {
                  continue;
                }
                double latitude = FileUtils.parseRequiredDouble(record.get(5));
                double longitude = FileUtils.parseRequiredDouble(record.get(6));

                manager.add(countryName, iso2Letter, iso3Letter, continent, latitude, longitude,
                    locale, friendlyName);

              } catch (RuntimeException e) {
                // CSVRecord has a very descriptive toString() implementation
                String logmsg = Messages.getMessage(LogCodes.WPH1023E, String.valueOf(record),
                    entry.getFilename(), e.getMessage());
                throw new KeyedRuntimeException(LogCodes.WPH1023E, logmsg, e);
              }
            }
          }
        }
      }

    } catch (IOException e) {
      logger.logError(LogCodes.WPH1013E, e);
      throw new RuntimeException(e);
    }

    return manager;
  }

  protected void add(String countryName, String iso2Letter, String iso3Letter, String continent,
      double latitude, double longitude, String countryCode, String friendlyName) {
    Country country = new Country(countryName, iso2Letter, iso3Letter, continent, latitude,
        longitude, countryCode, CountryNameSpecification.NAME);
    countryNames.add(country);
    countryNames.add(countryCode, country);
    if (friendlyName != null && !friendlyName.trim().isEmpty()) {
      country = new Country(countryName, iso2Letter, iso3Letter, continent, latitude, longitude,
          countryCode, CountryNameSpecification.NAME, friendlyName);
      countryNames.add(country);
      countryNames.add(countryCode, country);
    }
    country = new Country(countryName, iso2Letter, iso3Letter, continent, latitude, longitude,
        countryCode, CountryNameSpecification.ISO2);
    countryISO2Codes.add(country);
    countryISO2Codes.add(countryCode, country);
    country = new Country(countryName, iso2Letter, iso3Letter, continent, latitude, longitude,
        countryCode, CountryNameSpecification.ISO3);
    countryISO3Codes.add(country);
    countryISO3Codes.add(countryCode, country);
  }

  public String getPseudorandom(String identifier) {
    CountrySpecificationResourceManager manager = countryNames;
    Country country = getValue(identifier);
    if (country != null) {
      CountryNameSpecification spec = country.getCountryNameSpecification();
      if (spec == CountryNameSpecification.ISO2) {
        manager = countryISO2Codes;
      } else if (spec == CountryNameSpecification.ISO3) {
        manager = countryISO3Codes;
      }
    }
    return manager.getPseudorandom(identifier);
  }

  public String getRandomKey() {
    return countryNames.getRandomKey();
  }

  /**
   * Returns the identifier of a random country among the given number of countries closest to the
   * country identified by the given identifier. The returned identifier of the selected country
   * will be the one of the same type as the given identifier.
   *
   * @param identifier the identifier of the selected country
   * @param k the number of closest countries to the given country to use as candidates for
   *        selection
   * 
   * @return the identifier of the selected country or <i>null</i> if no country can be selected
   *         because the given identifier is not recognized or no candidate countries have been
   *         loaded.
   */
  public String getClosestCountry(String identifier, int k) {
    String selected = null;
    Country country = getValue(identifier);
    if (country != null) {
      CountryNameSpecification spec = country.getCountryNameSpecification();
      CountrySpecificationResourceManager manager = countryNames;
      if (spec == CountryNameSpecification.ISO2) {
        manager = countryISO2Codes;
      } else if (spec == CountryNameSpecification.ISO3) {
        manager = countryISO3Codes;
      }
      LatLonDistance<Country> distanceCalc = new LatLonDistance<>(manager.getValues());
      List<Country> nearest = distanceCalc.findNearestK(country, k);
      if (!nearest.isEmpty()) {
        Country selectedCountry = nearest.get(manager.getRandom().nextInt(nearest.size()));
        selected = selectedCountry.getName(selectedCountry.getCountryNameSpecification());
      }
    }
    return selected;
  }

  @Override
  public boolean isValidKey(String identifier) {
    return getValue(identifier) != null;
  }

  public Country getValue(String identifier) {
    Country country = countryNames.getValue(identifier);
    if (country == null) {
      country = countryISO2Codes.getValue(identifier);
      if (country == null) {
        country = countryISO3Codes.getValue(identifier);
      }
    }
    return country;
  }

  public Country getRandomValue() {
    return getRandomValue(CountryNameSpecification.NAME);
  }

  public Country getRandomValue(CountryNameSpecification spec) {
    Country random = null;
    if (spec == null) {
      spec = CountryNameSpecification.NAME;
    }
    switch (spec) {
      case ISO2:
        random = countryISO2Codes.getRandomValue();
        break;
      case ISO3:
        random = countryISO3Codes.getRandomValue();
        break;
      default:
        random = countryNames.getRandomValue();
    }
    return random;
  }
}
