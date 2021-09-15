/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

/**
 * Generic error to throw when the FPE encryption engine has a failure. The message from this
 * exception does not include any PHI and is safe to log.
 */
public class EncryptionEngineException extends Exception {

  private static final long serialVersionUID = 1L;

  public EncryptionEngineException(Throwable cause) {
    // cause exception is not controlled - do not use its message
    super(Messages.getMessage(LogCodes.WPH1013E, "encryption engine failure"), cause);
  }
}
