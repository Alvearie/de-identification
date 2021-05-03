/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

public class PrivacyProviderInvalidInputException extends RuntimeException {

  private static final long serialVersionUID = 3703087254325803213L;

  public static final String MESSAGE_ID = LogCodes.WPH1024E;

  public PrivacyProviderInvalidInputException(String input, String ruleName) {
    super(Messages.getMessage(MESSAGE_ID, input, ruleName));
  }
}
