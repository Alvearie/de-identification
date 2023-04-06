/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

public class WorldManufacturerId implements ManagedResource, Serializable {

  private static final long serialVersionUID = 4410502613835915628L;

  private final String id;
  private final String manufacturer;

  /**
   * Instantiates a new WorldManufacturerId.
   *
   * @param wmi the world manufacturer identifier (WMI)
   * @param manufacturer the manufacturer to whom the WMI is assigned
   * 
   * @throws IllegalArgumentException if any of the input values is null, whitespace, or, for the
   *         WMI, not of the correct length (3).
   */
  public WorldManufacturerId(String wmi, String manufacturer) {
    if (wmi == null || wmi.trim().length() != 3 || wmi.length() != 3) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(wmi), "WMI"));
    }
    if (manufacturer == null || manufacturer.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(manufacturer), "manufacturer"));
    }
    this.id = wmi.toUpperCase();
    this.manufacturer = manufacturer;
  }

  public String getId() {
    return id;
  }

  public String getManufacturer() {
    return manufacturer;
  }

  @Override
  public String getKey() {
    return id;
  }
}
