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
 * Class that represents a health care institution.
 */
public class Hospital implements LocalizedEntity, ManagedResource, Serializable {
  private static final long serialVersionUID = 6739890934720042286L;

  private final String name;
  private final String countryCode;

  /**
   * Instantiates a new Hospital.
   *
   * @param name the name of the institution
   * @param countryCode the country code or locale associated with this resource
   */
  public Hospital(String name, String countryCode) {
    if (name == null || name.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(name), "hospital name"));
    }
    if (countryCode == null || countryCode.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(countryCode), "hospital country"));
    }
    this.name = name;
    this.countryCode = countryCode;
  }

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

  @Override
  public String getKey() {
    return name.toUpperCase();
  }
}
