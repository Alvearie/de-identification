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
 * Class that represents a US-style postal (ZIP) code.
 */
public class ZIPCode implements ManagedResource, Serializable {

  private static final long serialVersionUID = -6838351521747179298L;

  private final String code;
  private final int population;

  /**
   * Creates a new ZIP code resource.
   * 
   * @param code the ZIP code
   * @param populationString the population of the geographic area covered by this ZIP code in
   *        string form
   * 
   * @throws IllegalArgumentException if any of the input is null, empty, or otherwise invalid
   */
  public ZIPCode(String code, String populationString) {
    if (code == null || code.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(code), "ZIPcode"));
    }
    if (populationString == null) {
      throw new IllegalArgumentException(Messages.getMessage(LogCodes.WPH1010E,
          String.valueOf(populationString), "ZIPcode population"));
    }
    int populationInt;
    try {
      populationInt = Integer.parseInt(populationString);
    } catch (NumberFormatException e) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, populationString, "ZIPcode population"));
    }
    if (populationInt < 0) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, populationString, "ZIPcode population"));
    }

    this.code = code;
    this.population = populationInt;
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
