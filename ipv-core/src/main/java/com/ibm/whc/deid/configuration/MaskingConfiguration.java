/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.configuration;

import java.util.Map;

/**
 * The interface Masking configuration.
 *
 */
public interface MaskingConfiguration {
  /**
   * Gets value.
   *
   * @param key the key
   * @return the value
   */
  Object getValue(String key);

  /**
   * Gets int value.
   *
   * @param key the key
   * @return the int value
   */
  int getIntValue(String key);

  /**
   * Gets boolean value.
   *
   * @param key the key
   * @return the boolean value
   */
  boolean getBooleanValue(String key);

  /**
   * Gets string value.
   *
   * @param key the key
   * @return the string value
   */
  String getStringValue(String key);

  Map<String, String> getStringValueWithPrefixMatch(String key);

  /**
   * Sets value.
   *
   * @param key the key
   * @param value the value
   */
  void setValue(String key, Object value);

}
