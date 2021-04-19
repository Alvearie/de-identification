/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import com.ibm.whc.deid.resources.ManagedResource;

/**
 * Object that holds information about a type/brand of credit card.
 */
public class CreditCardType implements ManagedResource {

  private final String name;
  private final String key;
  private final String[] prefixes;
  private final int minimumLength;
  private final int maximumLength;

  /**
   * Instantiates a new Credit card.
   *
   * @param name the name
   * @param prefixes the prefixes
   * @param minimumLength the minimum length
   * @param maximumLength the maximum length
   */
  public CreditCardType(String name, String[] prefixes, int minimumLength, int maximumLength) {
    this.name = name;
    this.key = name.toUpperCase();
    this.prefixes = prefixes;
    this.minimumLength = minimumLength;
    this.maximumLength = maximumLength;
  }

  /**
   * Gets minimum length.
   *
   * @return the minimum length
   */
  public int getMinimumLength() {
    return minimumLength;
  }

  /**
   * Gets maximum length.
   *
   * @return the maximum length
   */
  public int getMaximumLength() {
    return maximumLength;
  }

  /**
   * Get prefixes string [ ].
   *
   * @return the string [ ]
   */
  public String[] getPrefixes() {
    return prefixes;
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
    return key;
  }
}
