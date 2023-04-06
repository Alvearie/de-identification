/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.resources;

import java.io.Serializable;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

/**
 * A resource consisting solely of a string value and its key.
 */
public class KeyValueResource implements Serializable, ManagedResource {

  private static final long serialVersionUID = -5306850639448831252L;
  
  private final String key;
  private final String value;

  /**
   * Instantiates a new resource object.
   *
   * @param key the unique key for this resource
   * @param value the value of this resource
   * 
   * @throws IllegalArgumentException if the given key or value is null or whitespace.
   */
  public KeyValueResource(String key, String value) {
    if (key == null || key.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(key), "resource key"));
    }
    if (value == null || value.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(value), "resource value"));
    }
    this.key = key;    
    this.value = value;
  }

  /**
   *
   * @return the value
   */
  public String getValue() {
    return value;
  }

  @Override
  public String getKey() {
    return key;
  }
}
