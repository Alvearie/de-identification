/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

public class City implements Location, LocalizedEntity, Serializable, ManagedResource {

  private static final long serialVersionUID = 2457375277958856977L;

  private final String name;
  private final String key;
  private final String nameCountryCode;
  private final LatitudeLongitude latitudeLongitude;

  /**
   * Instantiates a new City.
   *
   * @param name the city name
   * @param latitude the latitude in string form - optional
   * @param longitude the longitude in string form - optional
   * @param nameCountryCode the name country code
   * 
   * @throws IllegalArgumentException if any of the required input values is null or whitespace or
   *         any provided input is invalid.
   */
  public City(String name, String latitude, String longitude, String nameCountryCode) {
    this.name = name;
    this.nameCountryCode = nameCountryCode;
    init();

    this.latitudeLongitude = LatitudeLongitude.buildLatitudeLongitude(latitude, longitude);
    
    this.key = name.toUpperCase();
  }

  /**
   * Instantiates a new City.
   *
   * @param name the city name
   * @param latitude the latitude
   * @param longitude the longitude
   * @param nameCountryCode the name country code
   * 
   * @throws IllegalArgumentException if any of the input values is null, whitespace, or invalid.
   */
  public City(String name, double latitude, double longitude, String nameCountryCode) {
    this.name = name;
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

  @Override
  public String getKey() {
    return key;
  }
}
