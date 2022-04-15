/*
 * (C) Copyright IBM Corp. 2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.exception;

import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

/*
 * An exception for handling invalid user input
 */
public class InvalidInputException extends Exception {

  private static final long serialVersionUID = -637284141128478682L;

  private String msgParm;

  // The message here is expected to be the invalid input field
  public InvalidInputException(String message) {
    super(Messages.getMessage(LogCodes.WPH6002E, message));
    msgParm = message;
  }

  public InvalidInputException(String message, Throwable cause) {
    super(Messages.getMessage(LogCodes.WPH6002E, message), cause);
    msgParm = message;
  }
  
  public String getMessageParameter() {
    return msgParm;
  }
}
