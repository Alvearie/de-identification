/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;

public class LastName implements LocalizedEntity, ManagedResource, Serializable {

  private static final long serialVersionUID = 7778507725528126383L;

  private final String name;
  private final String nameCountryCode;

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
    return name;
  }
}
