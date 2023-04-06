/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

public class OriginalMaskedValuePair {
  private final String original;
  private final String masked;

  /**
   * Gets masked.
   *
   * @return the masked
   */
  public String getMasked() {
    return masked;
  }

  /**
   * Gets original.
   *
   * @return the original
   */
  public String getOriginal() {
    return original;
  }

  /**
   * Instantiates a new Original masked value pair.
   *
   * @param original the original
   * @param masked the masked
   */
  public OriginalMaskedValuePair(String original, String masked) {
    this.original = original;
    this.masked = masked;
  }
}
