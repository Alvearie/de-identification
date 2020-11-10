/*
 * (C) Copyright IBM Corp. 2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.util;

/*
 * An exception for handling invalid masking configuration
 */
public class InvalidMaskingConfigurationException extends Exception {

  private static final long serialVersionUID = 8510810871073867687L;

  public InvalidMaskingConfigurationException(String message) {
    super(message);
  }

  public InvalidMaskingConfigurationException(String message, Throwable cause) {
    super(message, cause);
  }
}
