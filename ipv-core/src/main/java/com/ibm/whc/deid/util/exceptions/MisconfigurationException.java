/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util.exceptions;

public class MisconfigurationException extends RuntimeException {
  /** */
  private static final long serialVersionUID = 421652493855733898L;

  public MisconfigurationException(String message) {
    super(message);
  }
}
