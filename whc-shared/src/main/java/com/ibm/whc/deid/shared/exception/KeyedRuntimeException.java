/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.exception;

/**
 * Runtime exception that provides an error message key.
 */
public class KeyedRuntimeException extends RuntimeException implements Keyed {

  private static final long serialVersionUID = -52538381390881341L;

  private final String messageKey;
  
  public KeyedRuntimeException(String msgKey, String message) {
    super(message);
    messageKey = msgKey;
  }

  public KeyedRuntimeException(String msgKey, String message, Throwable cause) {
    super(message, cause);
    messageKey = msgKey;
  }

  @Override
  public String getMessageKey() {
    return messageKey;
  }
}
