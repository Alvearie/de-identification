/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;

public class SWIFTCode implements Serializable, ManagedResource {
  private static final long serialVersionUID = -3928533685856348015L;

  private final String code;
  private final Country country;

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

  @Override
  public String getKey() {
    return code.toUpperCase();
  }
}
