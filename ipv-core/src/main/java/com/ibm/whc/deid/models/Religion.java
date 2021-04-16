/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;

public class Religion implements LocalizedEntity, ManagedResource, Serializable {

  private static final long serialVersionUID = 5034883554471207043L;

  private final String name;
  private final String nameCountryCode;

  /**
   * Instantiates a new Religion.
   *
   * @param name the name
   * @param nameCountryCode the name country code
   */
  public Religion(String name, String nameCountryCode) {
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
    return name.toUpperCase();
  }
}
