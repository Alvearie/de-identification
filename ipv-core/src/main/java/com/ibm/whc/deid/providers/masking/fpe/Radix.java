/*
 * © Merative US L.P. 2021
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
  DIGITS("0123456789", 6, 56, '0'),

  /**
   * Lower case letters a-z
   */
  LOWER("abcdefghijklmnopqrstuvwxyz", 5, 40, 'a'),

  /**
   * Digits 0-9 and lower case letters a-z
   */
  DIGITS_LOWER("0123456789abcdefghijklmnopqrstuvwxyz", 4, 36, 'a'),

  /**
   * Upper case letters A-Z
   */
  UPPER("ABCDEFGHIJKLMNOPQRSTUVWXYZ", 5, 40, 'A'),

  /**
   * Digits 0-9 and upper case letters A-Z
   */
  DIGITS_UPPER("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", 4, 36, 'a');

  private final String value;
  private final int minchars;
  private final int maxchars;
  private final char padchar;

  private Radix(String val, int min, int max, char pad) {
    value = val;
    minchars = min;
    maxchars = max;
    padchar = pad;
  }

  /**
   * 
   * @return the number of different characters that are recognized and encrypted
   */
  public String value() {
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
