/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.exception;

public class KeyedIllegalArgumentException extends IllegalArgumentException implements Keyed {

  private static final long serialVersionUID = 177630315250572568L;

  private final String messageKey;

  public KeyedIllegalArgumentException(String message) {
    super(message);
    messageKey = null;
  }

  public KeyedIllegalArgumentException(String message, Throwable cause) {
    super(message, cause);
    messageKey = null;
  }

  public KeyedIllegalArgumentException(String msgKey, String message) {
    super(message);
    messageKey = msgKey;
  }

  public KeyedIllegalArgumentException(String msgKey, String message, Throwable cause) {
    super(message, cause);
    messageKey = msgKey;
  }

  @Override
  public String getMessageKey() {
    return messageKey;
  }
}
