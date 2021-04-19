/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;

public class County implements LocalizedEntity, ManagedResource, Serializable {

  private static final long serialVersionUID = 6632609422369704157L;

  private final String name;
  private final String nameCountryCode;
  private final String key;

  /**
   * Instantiates a new County.
   *
   * @param name the name
   * @param nameCountryCode the name country code
   */
  public County(String name, String nameCountryCode, String shortName, String state,
      Integer population, String key) {
    this.name = name;
    this.nameCountryCode = nameCountryCode;
    this.key = key;
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
