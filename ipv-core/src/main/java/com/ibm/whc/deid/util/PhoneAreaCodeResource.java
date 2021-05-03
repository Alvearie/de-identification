/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.Serializable;
import com.ibm.whc.deid.resources.ManagedResource;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

public class PhoneAreaCodeResource implements Serializable, ManagedResource {

  private static final long serialVersionUID = 5281829413045836683L;

  private final String code;

  /**
   * Instantiates a new telephone area code resource.
   *
   * @param code the area code as a string
   * 
   * @throws IllegalArgumentException if the given code is null or whitespace
   */
  public PhoneAreaCodeResource(String code) {
    try {
      if (code == null || code.trim().isEmpty() || Integer.parseInt(code) < 0) {
        throw new IllegalArgumentException(
            Messages.getMessage(LogCodes.WPH1010E, String.valueOf(code), "area code"));
      }
    } catch (NumberFormatException e) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(code), "area code"), e);
    }
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
