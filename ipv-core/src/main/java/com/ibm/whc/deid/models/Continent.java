/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

public class Continent implements Location, LocalizedEntity, Serializable, ManagedResource {

  private static final long serialVersionUID = -1487382551058089794L;

  private final String name;
  private final String nameCountryCode;
  private final LatitudeLongitude latitudeLongitude;

  /**
   * Instantiates a new Continent.
   *
   * @param name the name
   * @param nameCountryCode the locale code of the source data set
   * @param latitude the latitude in decimal format as a string
   * @param longitude the longitude in decimal format as a string
   */
  public Continent(String name, String nameCountryCode, String latitude, String longitude) {
    if (name == null || name.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(name), "continent name"));
    }
    if (nameCountryCode == null || nameCountryCode.trim().isEmpty()) {
      throw new IllegalArgumentException(Messages.getMessage(LogCodes.WPH1010E,
          String.valueOf(nameCountryCode), "continent locale"));
    }
    this.name = name;
    this.nameCountryCode = nameCountryCode;
    this.latitudeLongitude = new LatitudeLongitude(latitude, longitude);
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
