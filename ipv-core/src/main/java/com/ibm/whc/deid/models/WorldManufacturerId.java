/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;

public class WorldManufacturerId implements ManagedResource, Serializable {

  private static final long serialVersionUID = 4410502613835915628L;

  private final String id;
  private final String manufacturer;

  /**
   * Instantiates a new WorldManufacturerId.
   *
   * @param name the WMI 
   * @param manufacturer the manufacturer to whom the WMI is assigned
   */
  public WorldManufacturerId(String id, String manufacturer) {
    this.id = id.toUpperCase();
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
