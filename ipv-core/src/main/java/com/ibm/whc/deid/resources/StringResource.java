/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.resources;

import java.io.Serializable;

public class StringResource implements Serializable, ManagedResource {

  private static final long serialVersionUID = -26106879086124325L;
  
  private final String value;

  /**
   * Instantiates a new StringResource.
   *
   * @param value the string that represents a resource
   */
  public StringResource(String value) {
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
    return value.toUpperCase();
  }
}
