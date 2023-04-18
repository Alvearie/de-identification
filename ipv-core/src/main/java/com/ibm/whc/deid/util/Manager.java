/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

/**
 * Interface supported by classes that manage groups of items, such as cities, genders, names, religions, etc. 
 */
public interface Manager {

  /**
   * Is valid key boolean.
   *
   * @param identifier the identifier
   * @return the boolean
   */
  boolean isValidKey(String identifier);
}
