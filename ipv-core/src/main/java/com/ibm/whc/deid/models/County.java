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

/**
 * An object that describes a <i>county</i> - a political subdivision of a nation, generally the
 * subdivision contained within the sub-national subdivision (state, province).
 */
public class County implements LocalizedEntity, ManagedResource, Serializable {

  private static final long serialVersionUID = 6632609422369704157L;

  private final String name;
  private final String shortName;
  private final String nameCountryCode;
  private final boolean useFullNameAsKey;

  /**
   * Instantiates a new County resource.
   *
   * @param name the full or formal name of the county
   * @param nameCountryCode a code for the containing country or locale for this resource
   * @param shortName the shorter or informal name of the county
   * @param state the sub-national political authority that contains this county
   * @param population the population of this county in string format
   * @param useFullNameKey <i>true</i> if the unique key used to identify this resource should be
   *        based on the full, formal name of the county and <i>false</i> if it should be based on
   *        the short, informal name.
   * 
   * @throws IllegalArgumentException if any of the input is null, empty, or otherwise invalid
   */
  public County(String name, String nameCountryCode, String shortName, String state,
      String population, boolean useFullNameKey) {
    if (name == null || name.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(name), "county name"));
    }
    if (nameCountryCode == null || nameCountryCode.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(nameCountryCode), "county locale"));
    }
    if (shortName == null || shortName.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(shortName), "county short name"));
    }
    if (state == null || state.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(state), "county state"));
    }
    if (population == null) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(population), "county population"));
    }
    int populationInt;
    try {
      populationInt = Integer.parseInt(population);
    } catch (NumberFormatException e) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, population, "county population"));
    }
    if (populationInt < 0) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, population, "county population"));
    }

    this.name = name;
    this.shortName = shortName;
    this.nameCountryCode = nameCountryCode;
    this.useFullNameAsKey = useFullNameKey;
  }

  /**
   * Gets name country code.
   *
   * @return the country code / locale with which this resource is associated
   */
  @Override
  public String getNameCountryCode() {
    return nameCountryCode;
  }

  /**
   *
   * @return the full or formal name of the county
   */
  public String getName() {
    return name;
  }

  /**
   * 
   * @return the short or informal name of the county
   */
  public String getShortName() {
    return shortName;
  }

  @Override
  public String getKey() {
    return useFullNameAsKey ? name.toUpperCase() : shortName.toUpperCase();
  }

  public boolean isUseFullNameAsKey() {
    return useFullNameAsKey;
  }
}
