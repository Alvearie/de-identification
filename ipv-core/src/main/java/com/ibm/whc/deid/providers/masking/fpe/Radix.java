/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

public enum Radix {

  /**
   * Digits 0-9
   */
  DIGITS(10, 6, 56, '0'),

  /**
   * lower case letters a-z
   */
  LOWER(26, 5, 40, 'a'),

  /**
   * Digits 0-9 or lower case letters a-z
   */
  DIGITS_LOWER(36, 4, 36, 'a');

  private final int value;
  private final int minchars;
  private final int maxchars;
  private final char padchar;

  private Radix(int val, int min, int max, char pad) {
    value = val;
    minchars = min;
    maxchars = max;
    padchar = pad;
  }

  public int value() {
    return value;
  }

  public int getMinStringLength() {
    return minchars;
  }

  public int getMaxStringLength() {
    return maxchars;
  }

  public char getPadChar() {
    return padchar;
  }
}
