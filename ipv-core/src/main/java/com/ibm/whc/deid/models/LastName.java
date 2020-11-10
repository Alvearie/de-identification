/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;

public class LastName implements LocalizedEntity, Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 7778507725528126383L;
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
   * Instantiates a new Last name.
   *
   * @param name the name
   * @param nameCountryCode the name country code
   */
  public LastName(String name, String nameCountryCode) {
    this.name = name;
    this.nameCountryCode = nameCountryCode;
  }
}
