/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.exception;

/*
 * A class used to handle an exception during de-identification processing.
 */
public class DeidException extends Exception {
  /** */
  private static final long serialVersionUID = 8510810871073867687L;

  public DeidException(String message) {
    super(message);
  }

  public DeidException(String message, Throwable cause) {
    super(message, cause);
  }
}
