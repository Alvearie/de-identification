/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;

public class State implements LocalizedEntity, Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 8995106047926366446L;
	private final String name;
  private final String nameCountryCode;
  private final String abbreviation;
  private final StateNameFormat nameFormat;

  @Override
  public String toString() {
    if (nameFormat == StateNameFormat.ABBREVIATION) {
      return abbreviation;
    }

    return name;
  }

  public String toString(StateNameFormat nameFormat) {
    if (nameFormat == StateNameFormat.ABBREVIATION) {
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

  /**
   * Instantiates a new State.
   *
   * @param name the name
   * @param nameCountryCode the name country code
   * @param abbreviation the abbreviation
   * @param population the population
   */
  public State(String name, String nameCountryCode, String abbreviation, Long population,
      StateNameFormat nameFormat) {
    this.name = name;
    this.nameCountryCode = nameCountryCode;
    this.abbreviation = abbreviation;
    this.nameFormat = nameFormat;
  }
}
