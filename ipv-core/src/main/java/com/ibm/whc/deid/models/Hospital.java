/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;

public class Hospital implements LocalizedEntity, Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 6739890934720042286L;
	private final String name;
  private final String countryCode;

  /**
   * Gets country code.
   *
   * @return the country code
   */
  @Override
public String getNameCountryCode() {
    return countryCode;
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
   * Instantiates a new Hospital.
   *
   * @param name the name
   * @param countryCode the country code
   */
  public Hospital(String name, String countryCode) {
    this.name = name;
    this.countryCode = countryCode;
  }
}
