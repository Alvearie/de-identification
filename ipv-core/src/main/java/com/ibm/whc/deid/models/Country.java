/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import java.util.Arrays;

import com.ibm.whc.deid.util.CountryNameSpecification;

public class Country implements Location, LocalizedEntity, Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 3519356529649094497L;
	private String name;
  private String nameCountryCode;
  private String iso2code;
  private String iso3code;
  private String continent;
  private LatitudeLongitude latitudeLongitude;
  private Country[] neighbors;

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
   * Instantiates a new Country.
   *
   * @param name the name
   * @param iso2code the iso 2 code
   * @param iso3code the iso 3 code
   * @param continent the continent
   * @param latitude the latitude
   * @param longitude the longitude
   * @param nameCountryCode the name country code
   */
  public Country(String name, String iso2code, String iso3code, String continent, Double latitude,
      Double longitude, String nameCountryCode) {
    this.name = name;
    this.iso2code = iso2code;
    this.iso3code = iso3code;
    this.continent = continent;
    this.nameCountryCode = nameCountryCode;
    this.latitudeLongitude =
        new LatitudeLongitude(latitude, longitude, LatitudeLongitudeFormat.DECIMAL);
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
}
