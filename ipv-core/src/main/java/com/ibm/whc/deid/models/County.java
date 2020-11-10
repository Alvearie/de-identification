/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;

public class County implements LocalizedEntity, Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 6632609422369704157L;
	private final String name;
  private final String nameCountryCode;

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
   * Instantiates a new County.
   *
   * @param name the name
   * @param nameCountryCode the name country code
   */
  public County(String name, String nameCountryCode, String shortName, String state,
      Integer population) {
    this.name = name;
    this.nameCountryCode = nameCountryCode;
  }
}
