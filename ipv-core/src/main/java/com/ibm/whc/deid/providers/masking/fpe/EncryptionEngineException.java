/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

public class EncryptionEngineException extends Exception {

  private static final long serialVersionUID = 1L;

  public EncryptionEngineException(Throwable cause) {
    super(cause);
  }
}
