/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

public class UnsupportedLengthException extends Exception {

  private static final long serialVersionUID = 1L;

  public UnsupportedLengthException(int length) {
    super(Integer.toString(length));
  }
}
