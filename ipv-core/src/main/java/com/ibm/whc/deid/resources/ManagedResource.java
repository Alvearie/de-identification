/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.resources;

/**
 * An item of a particular category, such as a city, gender, religion, name, etc. that can be
 * uniquely identified within its category by a key.
 */
public interface ManagedResource {

  /**
   * 
   * @return the unique key within its category of items for this item
   */
  public String getKey();
}
