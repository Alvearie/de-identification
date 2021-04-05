/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;

public class PhoneAreaCodeResource implements Serializable, ManagedResource {

  private static final long serialVersionUID = 5281829413045836683L;

  private final String code;

  /**
   * Instantiates a new telephone area code resource.
   *
   * @param code the area code as a string
   */
  public PhoneAreaCodeResource(String code) {
    this.code = code;
  }

  public String getAreaCode() {
    return code;
  }

  @Override
  public String getKey() {
    return code;
  }
}
