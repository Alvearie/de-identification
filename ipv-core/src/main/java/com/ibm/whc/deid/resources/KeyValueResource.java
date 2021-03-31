/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.resources;

import java.io.Serializable;

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
   */
  public KeyValueResource(String key, String value) {
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
