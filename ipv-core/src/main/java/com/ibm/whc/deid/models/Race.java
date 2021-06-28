/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

/**
 * A resource that represents a race or ethnicity.
 */
public class Race implements LocalizedEntity, ManagedResource, Serializable {

  private static final long serialVersionUID = -7426090366511879997L;

  private final String name;
  private final String nameCountryCode;

  /**
   * Instantiates a new Race.
   *
   * @param name the identifier of the race or ethnicity
   * @param nameCountryCode a code for the containing country or locale for this resource
   * 
   * @throws IllegalArgumentException if any of the input is null, empty, or otherwise invalid
   */
  public Race(String name, String nameCountryCode) {
    if (name == null || name.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(name), "race"));
    }
    if (nameCountryCode == null || nameCountryCode.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(nameCountryCode), "race locale"));
    }
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
