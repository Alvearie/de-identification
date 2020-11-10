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

  private static final long serialVersionUID = -6095243896971570680L;

  // The message here is expected to be the invalid input field
  public InvalidInputException(String message) {
    super(Messages.getMessage(LogCodes.WPH6002E, message));
  }

  public InvalidInputException(String message, Throwable cause) {
    super(Messages.getMessage(LogCodes.WPH6002E, message), cause);
  }
}
