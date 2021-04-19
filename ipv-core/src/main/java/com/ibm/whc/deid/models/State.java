/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;

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
   * @param nameCountryCode the name country code for localized states
   * @param abbreviation the abbreviation
   * @param population the population, which may be <i>null</i>
   * @param nameFormat the format by which this object is recognized
   * @param key the key used to identify this state object
   */
  public State(String name, String nameCountryCode, String abbreviation, Long population,
      StateNameFormat nameFormat, String key) {
    this.name = name;
    this.nameCountryCode = nameCountryCode;
    this.abbreviation = abbreviation;
    this.nameFormat = nameFormat;
    this.key = key;
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
