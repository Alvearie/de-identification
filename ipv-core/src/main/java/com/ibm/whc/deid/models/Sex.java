/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;

public class Sex implements LocalizedEntity, ManagedResource, Serializable {

  private static final long serialVersionUID = 2420465901225331134L;

  private final String name;
  private final String nameCountryCode;

  /**
   * Instantiates a new Sex.
   *
   * @param name the name
   * @param nameCountryCode the name country code
   */
  public Sex(String name, String nameCountryCode) {
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
