/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.exception;

/**
 * Interface supported by exception classes that provide a message identifier key.
 */
public interface Keyed {

  /**
   * 
   * @return the error message key
   */
  public String getMessageKey();
}
