/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import com.ibm.whc.deid.resources.ManagedResource;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

/**
 * Object that holds information about a type/brand of credit card.
 */
public class CreditCardType implements ManagedResource {

  private final String name;
  private final String[] prefixes;
  private final int minimumLength;
  private final int maximumLength;

  /**
   * Instantiates a new Credit card.
   *
   * @param name the credit card type name
   * @param prefixList the list of prefixes used by this credit card type separated by colons (:)
   * @param minimumLength the minimum length of this type of credit card number in string form
   * @param maximumLength the maximum length of this type of credit card number in string form
   */
  public CreditCardType(String name, String prefixList, String minimumLength,
      String maximumLength) {

    if (name == null || name.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(name), "card type name"));
    }

    if (prefixList == null || prefixList.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(prefixList), "card prefixes"));
    }
    String[] prefixes = prefixList.split(":");
    for (String prefix : prefixes) {
      if (prefix.trim().isEmpty()) {
        throw new IllegalArgumentException(
            Messages.getMessage(LogCodes.WPH1010E, String.valueOf(prefixList), "card prefixes"));
      }
    }
    if (prefixes.length == 0) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(prefixList), "card prefixes"));
    }

    if (minimumLength == null) {
      throw new IllegalArgumentException(Messages.getMessage(LogCodes.WPH1010E,
          String.valueOf(minimumLength), "card minimum length"));
    }
    int minLength;
    try {
      minLength = Integer.parseInt(minimumLength);
    } catch (NumberFormatException e) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, minimumLength, "card minimum length"));
    }
    if (minLength < 1) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, minimumLength, "card minimum length"));
    }

    if (maximumLength == null) {
      throw new IllegalArgumentException(Messages.getMessage(LogCodes.WPH1010E,
          String.valueOf(maximumLength), "card maximum length"));
    }
    int maxLength;
    try {
      maxLength = Integer.parseInt(maximumLength);
    } catch (NumberFormatException e) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, maximumLength, "card maximum length"));
    }
    if (maxLength < minLength) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, maximumLength, "card maximum length"));
    }

    this.name = name;
    this.prefixes = prefixes;
    this.minimumLength = minLength;
    this.maximumLength = maxLength;
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
   * Get card number prefixes used by this credit card type.
   *
   * @return the card number prefixes
   */
  public String[] getPrefixes() {
    String[] out = new String[prefixes.length];
    System.arraycopy(prefixes, 0, out, 0, prefixes.length);
    return out;
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
    return name.toUpperCase();
  }
}
