/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

public interface Manager {

  /**
   * Is valid key boolean.
   *
   * @param identifier the identifier
   * @return the boolean
   */
  boolean isValidKey(String identifier);

  /**
   * Gets random key.
   *
   * @return the random key
   */
  String getRandomKey();
}
