/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;

public class ZIPCode implements Serializable {
  /** */
  private static final long serialVersionUID = -6838351521747179298L;

  public String getCode() {
    return code;
  }

  public Integer getPopulation() {
    return population;
  }

  private final String code;
  private final Integer population;

  public ZIPCode(String code, Integer population) {
    this.code = code;
    this.population = population;
  }
}
