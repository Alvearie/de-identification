/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

/**
 * Defines the set of different characters that can be encrypted. Based on the number of characters
 * in the set, a minimum and maximum number of input characters are supported.
 */
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
   * Digits 0-9 and lower case letters a-z
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

  /**
   * 
   * @return the number of different characters that are recognized and encrypted
   */
  public int value() {
    return value;
  }

  /**
   * 
   * @return the minimum number of input characters that the encryption engine can support
   */
  public int getMinStringLength() {
    return minchars;
  }

  /**
   * 
   * @return the maximum number of input characters that the encryption engine can support
   */
  public int getMaxStringLength() {
    return maxchars;
  }

  /**
   * 
   * @return the character that is added to the input if additional characters are required to meet
   *         the minimum input requirements and if padding of additional characters is requested
   */
  public char getPadChar() {
    return padchar;
  }
}
