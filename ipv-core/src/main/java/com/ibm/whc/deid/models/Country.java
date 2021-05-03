/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import java.util.Arrays;
import com.ibm.whc.deid.resources.ManagedResource;
import com.ibm.whc.deid.util.CountryNameSpecification;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

public class Country implements Location, LocalizedEntity, ManagedResource, Serializable {

  private static final long serialVersionUID = 3519356529649094497L;

  private final String name;
  private final String nameCountryCode;
  private final String iso2code;
  private final String iso3code;
  private final String continent;
  private final CountryNameSpecification specification;
  private final LatitudeLongitude latitudeLongitude;
  private final String key;

  private Country[] neighbors;

  /**
   * Instantiates a new Country.
   *
   * @param name the name of the country
   * @param iso2code the ISO 2-char country code - optional
   * @param iso3code the ISO 3-code country code - optional
   * @param continent the continent - optional
   * @param latitude the latitude in string format - optional
   * @param longitude the longitude in string format - optional
   * @param nameCountryCode the locale
   * @param specification the type of identifier used to find this resource
   * 
   * @throws IllegalArgumentException if any of the required input values is null or whitespace or
   *         any provided input is invalid.
   */
  public Country(String name, String iso2code, String iso3code, String continent, String latitude,
      String longitude, String nameCountryCode, CountryNameSpecification specification) {
    this(name, iso2code, iso3code, continent, latitude, longitude, nameCountryCode, specification,
        null);
  }

  /**
   * Instantiates a new Country.
   *
   * @param name the name of the country
   * @param iso2code the ISO 2-char country code - optional
   * @param iso3code the ISO 3-code country code - optional
   * @param continent the continent - optional
   * @param latitude the latitude in string format - optional
   * @param longitude the longitude in string format - optional
   * @param nameCountryCode the locale
   * @param specification the type of identifier used to find this resource
   * @param key a unique key used to find this resource - if <i>null</i> the value is generated
   * 
   * @throws IllegalArgumentException if any of the required input values is null or whitespace or
   *         any provided input is invalid.
   */
  public Country(String name, String iso2code, String iso3code, String continent, String latitude,
      String longitude, String nameCountryCode, CountryNameSpecification specification,
      String key) {
    if (name == null || name.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(name), "country name"));
    }
    if (iso2code != null) {
      if (iso2code.trim().isEmpty()) {
        iso2code = null;
      } else if (iso2code.length() != 2 || iso2code.trim().length() != 2) {
        throw new IllegalArgumentException(
            Messages.getMessage(LogCodes.WPH1010E, String.valueOf(iso2code), "country iso2code"));
      }
    }
    if (iso3code != null) {
      if (iso3code.trim().isEmpty()) {
        iso3code = null;
      } else if (iso3code.length() != 3 || iso3code.trim().length() != 3) {
        throw new IllegalArgumentException(
            Messages.getMessage(LogCodes.WPH1010E, String.valueOf(iso3code), "country iso3code"));
      }
    }
    if (nameCountryCode == null || nameCountryCode.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(nameCountryCode),
              "country locale"));
    }
    if (specification == null) {
      throw new IllegalArgumentException(Messages.getMessage(LogCodes.WPH1010E,
          String.valueOf(specification), "country identifier specification"));
    }

    this.name = name;
    this.iso2code = iso2code == null ? null : iso2code.toUpperCase();
    this.iso3code = iso3code == null ? null : iso3code.toUpperCase();
    this.continent = continent;
    this.nameCountryCode = nameCountryCode;
    this.specification = specification;
    this.latitudeLongitude = LatitudeLongitude.buildLatitudeLongitude(latitude, longitude);

    if (key == null || key.trim().isEmpty()) {
      switch (specification) {
        case ISO2:
          key = iso2code;
          break;
        case ISO3:
          key = iso3code;
          break;
        case NAME:
        default:
          key = name.toUpperCase();
      }
    } else {
      key = key.toUpperCase();
    }
    this.key = key;
  }

  /**
   * Gets name country code.
   *
   * @return the name country code
   */
  @Override
  public String getNameCountryCode() {
    return nameCountryCode;
  }

  /**
   * Get neighbors country [ ].
   *
   * @return the country [ ]
   */
  public Country[] getNeighbors() {
    return neighbors;
  }

  /**
   * Sets neighbors.
   *
   * @param neighbors the neighbors
   */
  public void setNeighbors(final Country[] neighbors) {
    this.neighbors = Arrays.copyOf(neighbors, neighbors.length);
  }

  /**
   * Gets continent.
   *
   * @return the continent
   */
  public String getContinent() {
    return this.continent;
  }

  @Override
  public LatitudeLongitude getLocation() {
    return this.latitudeLongitude;
  }

  /**
   * Gets name.
   *
   * @return the name
   */
  public String getName() {
    return this.name;
  }

  /**
   * Gets name.
   *
   * @param spec the spec
   * @return the name
   */
  public String getName(CountryNameSpecification spec) {
    switch (spec) {
      case ISO2:
        return iso2code;
      case ISO3:
        return iso3code;
      case NAME:
      default:
        return name;
    }
  }

  public CountryNameSpecification getCountryNameSpecification() {
    return specification;
  }

  @Override
  public String getKey() {
    return key;
  }
}
