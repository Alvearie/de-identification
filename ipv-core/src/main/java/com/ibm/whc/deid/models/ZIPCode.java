/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;

public class ZIPCode implements ManagedResource, Serializable {

  private static final long serialVersionUID = -6838351521747179298L;

  private final String code;
  private final int population;

  public ZIPCode(String code, int population) {
    this.code = code;
    this.population = population;
  }

  public String getCode() {
    return code;
  }

  public int getPopulation() {
    return population;
  }

  @Override
  public String getKey() {
    return code;
  }
}
