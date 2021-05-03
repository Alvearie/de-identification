/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import java.util.List;
import com.ibm.whc.deid.resources.ManagedResource;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

public class City implements Location, LocalizedEntity, Serializable, ManagedResource {

  private static final long serialVersionUID = 2457375277958856977L;

  private final String name;
  private final String key;
  private String nameCountryCode;
  private String countryCode;
  private LatitudeLongitude latitudeLongitude;
  private List<City> neighbors;

  /**
   * Instantiates a new City.
   *
   * @param name the city name
   * @param latitude the latitude in string form
   * @param longitude the longitude in string form
   * @param countryCode the country code
   * @param nameCountryCode the name country code
   * 
   * @throws IllegalArgumentException if any of the input values is null, whitespace, or invalid.
   */
  public City(String name, String latitude, String longitude, String countryCode,
      String nameCountryCode) {
    this.name = name;
    this.countryCode = countryCode;
    this.nameCountryCode = nameCountryCode;
    init();

    this.latitudeLongitude = new LatitudeLongitude(latitude, longitude);

    this.key = name.toUpperCase();
  }

  /**
   * Instantiates a new City.
   *
   * @param name the city name
   * @param latitude the latitude
   * @param longitude the longitude
   * @param countryCode the country code
   * @param nameCountryCode the name country code
   * 
   * @throws IllegalArgumentException if any of the input values is null, whitespace, or invalid.
   */
  public City(String name, double latitude, double longitude, String countryCode,
      String nameCountryCode) {
    this.name = name;
    this.countryCode = countryCode;
    this.nameCountryCode = nameCountryCode;
    init();

    this.latitudeLongitude =
        new LatitudeLongitude(latitude, longitude, LatitudeLongitudeFormat.DECIMAL);

    this.key = name.toUpperCase();
  }

  private void init() {
    if (name == null || name.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(name), "city name"));
    }
    if (countryCode == null || countryCode.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(countryCode), "city country"));
    }
    if (nameCountryCode == null || nameCountryCode.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(nameCountryCode), "city locale"));
    }
  }

  /**
   * Gets name country code.
   *
   * @return the name country code
   */
  @Override
  public String getNameCountryCode() {
    return this.nameCountryCode;
  }

  /**
   * Gets neighbors.
   *
   * @return the neighbors
   */
  public List<City> getNeighbors() {
    return neighbors;
  }

  /**
   * Sets neighbors.
   *
   * @param neighbors the neighbors
   */
  public void setNeighbors(List<City> neighbors) {
    this.neighbors = neighbors;
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
   * Gets country code.
   *
   * @return the country code
   */
  public String getCountryCode() {
    return this.countryCode;
  }

  @Override
  public String getKey() {
    return key;
  }
}
