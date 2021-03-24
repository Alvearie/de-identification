/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;

public class Continent implements Location, LocalizedEntity, Serializable, ManagedResource {

  private static final long serialVersionUID = -1487382551058089794L;

  private final String name;
  private final String nameCountryCode;
  private final LatitudeLongitude latitudeLongitude;

  /**
   * Instantiates a new Continent.
   *
   * @param name the name
   * @param nameCountryCode the name country code
   * @param latitude the latitude
   * @param longitude the longitude
   */
  public Continent(String name, String nameCountryCode, Double latitude, Double longitude) {
    this.name = name;
    this.nameCountryCode = nameCountryCode;
    this.latitudeLongitude =
        new LatitudeLongitude(latitude, longitude, LatitudeLongitudeFormat.DECIMAL);
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
   * Gets name.
   *
   * @return the name
   */
  public String getName() {
    return name;
  }

  @Override
  public LatitudeLongitude getLocation() {
    return this.latitudeLongitude;
  }

  @Override
  public String getKey() {
    return name.toUpperCase();
  }
}
