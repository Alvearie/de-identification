/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

/**
 * Exception thrown when the input is not of appropriate length for the current FPE encryption
 * operation. The message from this exception does not include any PHI and is safe to log.
 */
public class UnsupportedLengthException extends Exception {

  private static final long serialVersionUID = 1L;

  private final int length;
  private final int min;
  private final int max;

  public UnsupportedLengthException(int length, int min, int max) {
    super(Messages.getMessage(LogCodes.WPH1027E, String.valueOf(min), String.valueOf(max),
        String.valueOf(length)));
    this.length = length;
    this.min = min;
    this.max = max;
  }

  public int getLength() {
    return length;
  }

  public int getMin() {
    return min;
  }

  public int getMax() {
    return max;
  }
}
