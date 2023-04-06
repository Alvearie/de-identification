/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import com.ibm.whc.deid.resources.ManagedResource;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

/**
 * Class that represents an occupation resource.
 * 
 * <p>
 * NOTE - This class is not thread-safe if categories are modified after multi-threaded usage
 * begins.
 */
public class Occupation implements LocalizedEntity, ManagedResource, Serializable {
  private static final long serialVersionUID = 7889120137630375876L;

  private final String name;
  private final String nameCountryCode;
  private final List<String> categories;

  /**
   * Instantiates a new Occupation.
   *
   * @param name the name of the occupation
   * @param nameCountryCode a code for the containing country or locale for this resource
   * @param category a category of occupations to which this occupation belongs
   * 
   * @throws IllegalArgumentException if any of the input is null, empty, or otherwise invalid
   */
  public Occupation(String name, String nameCountryCode, String category) {
    if (name == null || name.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(name), "occupation name"));
    }
    if (nameCountryCode == null || nameCountryCode.trim().isEmpty()) {
      throw new IllegalArgumentException(Messages.getMessage(LogCodes.WPH1010E,
          String.valueOf(nameCountryCode), "occupation locale"));
    }
    if (category == null || category.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(category), "occupation category"));
    }
    this.name = name;
    this.categories = new ArrayList<>();
    this.categories.add(category);
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
   * Gets the categories of occupations to which this occupation belongs.
   *
   * <p>
   * As a performance enhancement the actual list is returned instead of a copy. Therefore, the
   * contents of the returned list should not be modified after multi-threaded access begins to
   * maintain thread-safety.
   * 
   * @return the categories
   */
  public List<String> getCategories() {
    return categories;
  }

  /**
   * Adds an additional category to this occupation.
   * 
   * <p>
   * This method should not be called after multi-threaded access begins to maintain thread-safety.
   * 
   * @param category the additional category to add to this occupation
   * 
   * @throws IllegalArgumentException if the category is null or whitespace
   */
  public void addCategory(String category) {
    if (category == null || category.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(category), "occupation category"));
    }
    categories.add(category);
  }

  @Override
  public String getKey() {
    return name.toUpperCase();
  }
}
