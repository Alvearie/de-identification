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

public class PostalCode implements Location, ManagedResource, Serializable {

  private static final long serialVersionUID = 5510030912035883146L;

  private String code;
  private LatitudeLongitude latitudeLongitude;

  /**
   * Instantiates a new Postal code.
   *
   * @param code the code
   * @param latitude the latitude in string format
   * @param longitude the longitude in string format
   */
  public PostalCode(String code, String latitude, String longitude) {
    if (code == null || code.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(code), "postal code"));
    }
    this.code = code;
    this.latitudeLongitude = new LatitudeLongitude(latitude, longitude);
  }

  @Override
  public LatitudeLongitude getLocation() {
    return this.latitudeLongitude;
  }

  /**
   * Gets name.
   *
   * @return the name
   */
  public String getName() {
    return this.code;
  }

  @Override
  public String getKey() {
    return getName();
  }
}
