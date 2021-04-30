/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import java.util.regex.Pattern;
import com.ibm.whc.deid.resources.ManagedResource;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

public class SWIFTCode implements Serializable, ManagedResource {
  private static final long serialVersionUID = -3928533685856348015L;

  // country code is offsets 4 and 5
  public static final Pattern SWIFTCODE_PATTERN =
      Pattern.compile("[A-Z0-9]{4}[A-Z]{2}[A-Z0-9]{2}(?:[A-Z0-9]{3})?");

  private final String code;

  /**
   * Instantiates a new Swift code.
   *
   * @param code the code
   * 
   * @throws IllegalArgumentException if the given value is not in the expected SWIFTCode format
   * 
   * @see #SWIFTCODE_PATTERN
   */
  public SWIFTCode(String code) throws IllegalArgumentException {
    if (code == null) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, "null", "SWIFT code"));
    }
    this.code = code.toUpperCase();
    if (!SWIFTCODE_PATTERN.matcher(this.code).matches()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, code, "SWIFT code"));
    }
  }

  /**
   * Gets code.
   *
   * @return the code
   */
  public String getCode() {
    return code;
  }

  /**
   * Gets country.
   *
   * @return the country
   */
  public String getCountry() {
    return code.substring(4, 6);
  }

  @Override
  public String getKey() {
    return code;
  }
}
