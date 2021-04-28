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
   * @param iso2code the ISO 2-char country code
   * @param iso3code the ISO 3-code country code
   * @param continent the continent
   * @param latitude the latitude
   * @param longitude the longitude
   * @param nameCountryCode the locale
   * @param specification the type of identifier used to find this resource
   */
  public Country(String name, String iso2code, String iso3code, String continent, double latitude,
      double longitude, String nameCountryCode, CountryNameSpecification specification) {
    this(name, iso2code, iso3code, continent, latitude, longitude, nameCountryCode, specification,
        null);
  }

  /**
   * Instantiates a new Country.
   *
   * @param name the name of the country
   * @param iso2code the ISO 2-char country code
   * @param iso3code the ISO 3-code country code
   * @param continent the continent
   * @param latitude the latitude
   * @param longitude the longitude
   * @param nameCountryCode the locale
   * @param specification the type of identifier used to find this resource
   * @param key a unique key used to find this resource - if <i>null</i> the value is generated
   */
  public Country(String name, String iso2code, String iso3code, String continent, double latitude,
      double longitude, String nameCountryCode, CountryNameSpecification specification,
      String key) {
    if (name == null || name.trim().isEmpty()) {
      throw new IllegalArgumentException("country name is missing");
    }
    if (iso2code == null || iso2code.length() != 2) {
      throw new IllegalArgumentException("country iso2code is incorrect length");
    }
    if (iso3code == null || iso3code.length() != 3) {
      throw new IllegalArgumentException("country iso3code is incorrect length");
    }
    if (continent == null || continent.trim().isEmpty()) {
      throw new IllegalArgumentException("country continent is missing");
    }
    if (nameCountryCode == null || nameCountryCode.trim().isEmpty()) {
      throw new IllegalArgumentException("country locale is missing");
    }
    if (specification == null) {
      throw new IllegalArgumentException("country identifier specification is missing");
    }

    this.name = name;
    this.iso2code = iso2code.toUpperCase();
    this.iso3code = iso3code.toUpperCase();
    this.continent = continent;
    this.nameCountryCode = nameCountryCode;
    this.specification = specification;
    this.latitudeLongitude =
        new LatitudeLongitude(latitude, longitude, LatitudeLongitudeFormat.DECIMAL);

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
