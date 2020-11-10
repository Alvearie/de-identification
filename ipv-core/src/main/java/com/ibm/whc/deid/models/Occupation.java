/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import java.util.List;

public class Occupation implements LocalizedEntity, Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 7889120137630375876L;
	private final String nameCountryCode;
  private final List<String> categories;

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

  /**
   * Instantiates a new Occupation.
   *
   * @param name the name
   * @param nameCountryCode the name country code
   * @param categories the categories
   */
  public Occupation(String name, String nameCountryCode, List<String> categories) {
    this.categories = categories;
    this.nameCountryCode = nameCountryCode;
  }
}
