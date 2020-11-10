/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;

public class SWIFTCode implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -3928533685856348015L;
	private final String code;
  private final Country country;

  /**
   * Gets code.
   *
   * @return the code
   */
  public String getCode() {
    return code;
  }

  /**
   * Gets country.
   *
   * @return the country
   */
  public Country getCountry() {
    return country;
  }

  /**
   * Instantiates a new Swift code.
   *
   * @param code the code
   * @param country the country
   */
  public SWIFTCode(String code, Country country) {
    this.code = code;
    this.country = country;
  }
}
