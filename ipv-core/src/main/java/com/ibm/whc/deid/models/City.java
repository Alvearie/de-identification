/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import java.util.List;
import com.ibm.whc.deid.resources.Resource;

public class City implements Location, LocalizedEntity, Serializable, Resource {

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
   * @param name the name
   * @param latitude the latitude
   * @param longitude the longitude
   * @param countryCode the country code
   * @param nameCountryCode the name country code
   */
  public City(String name, Double latitude, Double longitude, String countryCode,
      String nameCountryCode) {
    this.name = name;
    this.countryCode = countryCode;
    this.nameCountryCode = nameCountryCode;

    this.latitudeLongitude =
        new LatitudeLongitude(latitude, longitude, LatitudeLongitudeFormat.DECIMAL);

    this.key = name.toUpperCase();
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
