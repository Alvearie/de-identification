/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import java.util.List;
import com.ibm.whc.deid.resources.ManagedResource;

public class Occupation implements LocalizedEntity, ManagedResource, Serializable {
  private static final long serialVersionUID = 7889120137630375876L;

  private final String name;
  private final String nameCountryCode;
  private final List<String> categories;

  /**
   * Instantiates a new Occupation.
   *
   * @param name the name
   * @param nameCountryCode the name country code
   * @param categories the categories
   */
  public Occupation(String name, String nameCountryCode, List<String> categories) {
    this.name = name;
    this.categories = categories;
    this.nameCountryCode = nameCountryCode;
  }

  public String getName() {
    return name;
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
   * Gets categories.
   *
   * @return the categories
   */
  public List<String> getCategories() {
    return categories;
  }

  @Override
  public String getKey() {
    return name.toUpperCase();
  }
}
