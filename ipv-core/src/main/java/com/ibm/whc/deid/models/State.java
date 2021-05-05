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
 * An object that describes the values of the first political subdivision of a nation, generally a
 * state or province.
 */
public class State implements LocalizedEntity, ManagedResource, Serializable {
  private static final long serialVersionUID = 8995106047926366446L;

  private final String name;
  private final String nameCountryCode;
  private final String abbreviation;
  private final StateNameFormat nameFormat;
  private final String key;

  /**
   * Instantiates a new State.
   *
   * @param name the name of the state
   * @param nameCountryCode a code for the containing country or locale for this resource
   * @param abbreviation the standard abbreviation of the state
   * @param nameFormat the format by which this resource is recognized
   * @param key the key used to identify this resource
   */
  public State(String name, String nameCountryCode, String abbreviation, StateNameFormat nameFormat,
      String key) {
    if (name == null || name.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(name), "state name"));
    }
    if (nameCountryCode == null || nameCountryCode.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(nameCountryCode), "state locale"));
    }
    if (abbreviation == null || abbreviation.trim().isEmpty()) {
      throw new IllegalArgumentException(Messages.getMessage(LogCodes.WPH1010E,
          String.valueOf(abbreviation), "state abbreviation"));
    }
    if (nameFormat == null) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, "null", "state name format"));
    }
    if (key == null || key.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(key), "state key"));
    }

    this.name = name;
    this.nameCountryCode = nameCountryCode;
    this.abbreviation = abbreviation;
    this.nameFormat = nameFormat;
    this.key = key.toUpperCase();
  }

  @Override
  public String toString() {
    return toString(null);
  }

  public String toString(StateNameFormat format) {
    if (format == StateNameFormat.ABBREVIATION) {
      return abbreviation;
    }
    return name;
  }

  public StateNameFormat getNameFormat() {
    return nameFormat;
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
  public String getKey() {
    return key;
  }
}
